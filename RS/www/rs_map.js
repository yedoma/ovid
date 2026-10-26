// www/rs_map.js  --  The map on the Map page (Leaflet, no R package needed). Part of Ovid Core // RS.
// The server sends what to draw with session$sendCustomMessage("rsmap", ...); a click on a flight sets input$map_pick.
// Line style says where a shape comes from: dashed = planned, solid = observed, dotted outline = derived by Ovid.
(function () {
  let map, base, lay = {}, sel = null, fitted = false, boxes = {}, last = null, legend;
  // ---- styles people can change: colour and size per layer, colour-by for flights, fill opacity; kept in this browser ----
  const DEF = { by: 'status', op: 0.15, col: {}, sz: {} }, KEY = 'ovidrs.mapstyle';
  let S = (() => { try { return Object.assign({}, DEF, JSON.parse(localStorage.getItem(KEY) || '{}')); } catch (e) { return { ...DEF }; } })();
  const save = () => { try { localStorage.setItem(KEY, JSON.stringify(S)); } catch (e) {} };
  const items = {};                              // layer name -> [{m, col, w, r, fill}] as drawn, so restyling needs no redraw
  const sz = g => S.sz[g] || 1, colOf = (g, c) => ((FL.includes(g) && S.by !== 'single') || g === YF) ? c : (S.col[g] || c);   // year outlines keep their kind's colour
  function reg(g, m, o) { (items[g] = items[g] || []).push({ m, ...o }); style1(g, items[g][items[g].length - 1]); return m; }
  function style1(g, it) {
    const k = sz(g), c = colOf(g, it.col), o = {};
    if (it.w !== undefined) o.weight = it.w * k;
    if (it.dot) { o.fillColor = c; o.color = it.edge || c; } else o.color = c;
    if (it.fill) o.fillOpacity = it.sel ? Math.min(1, S.op * 2) : S.op;
    if (it.r !== undefined && it.m.setRadius) it.m.setRadius(it.r * k);
    it.m.setStyle(o);
  }
  const restyle = () => { Object.entries(items).forEach(([g, a]) => a.forEach(it => style1(g, it))); drawLegend(); };
  const clear = g => { lay[g].clearLayers(); items[g] = []; };
  function init() {
    map = L.map('rsmap', { preferCanvas: true, zoomControl: true }).setView([0, 0], 2);   // the world; fitted to the data once it is drawn
    base = {
      'Satellite (Esri)': L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', { maxZoom: 21, maxNativeZoom: 19, attribution: 'Esri World Imagery' })
    };
    base['Satellite (Esri)'].addTo(map);
    map.createPane('datasets').style.zIndex = 350;   // above the imagery tiles, under the flights
    FL.concat([YF], DSL).forEach(k => { lay[k] = L.layerGroup().addTo(map); items[k] = []; });
    L.control.layers(null, lay, { collapsed: true }).addTo(map);
    styleControl(); legend = L.control({ position: 'bottomright' }); legend.onAdd = () => L.DomUtil.create('div', 'map-legend'); legend.addTo(map);
    OvidMapTools(map, { legend: () => legend.getContainer(), name: 'map', key: 'main' });   // Legend on/off and 250 ppi PNG (www/map_tools.js)
    map.on('overlayadd overlayremove', drawLegend);
  }
  // the Style panel (top left, opens on click): colour-by, fill, and colour + size per layer within sensible limits (0.5x to 3x)
  function styleControl() {
    const c = L.control({ position: 'topleft' });
    c.onAdd = () => {
      const d = L.DomUtil.create('div', 'map-style leaflet-bar'); L.DomEvent.disableClickPropagation(d); L.DomEvent.disableScrollPropagation(d);
      const rowsH = FL.concat([YF], DSL).map(g => `<div class="ms-row" data-g="${g}"><span>${tip(g)}</span><input type="color"><input type="range" min="0.5" max="3" step="0.25"></div>`).join('');
      d.innerHTML = `<a href="#" class="ms-btn" title="Colours and sizes">Style</a><div class="ms-body" hidden>
        <label>Colour flights by <select class="ms-by">${['status|Positioning', 'year|Year', 'sensor|Sensor', 'aircraft|Aircraft', 'site|Site', 'single|One colour'].map(o => { const [v, t] = o.split('|'); return `<option value="${v}">${t}</option>`; }).join('')}</select></label>
        <label>Fill <input type="range" class="ms-op" min="0" max="0.6" step="0.05"></label>
        <div class="ms-head"><span>Layer</span><span>Colour</span><span>Size</span></div>${rowsH}
        <a href="#" class="ms-reset">Reset to defaults</a></div>`;
      const q = s => d.querySelector(s), body = q('.ms-body');
      q('.ms-btn').onclick = e => { e.preventDefault(); body.hidden = !body.hidden; sync(); };
      q('.ms-by').onchange = e => { S.by = e.target.value; save(); recolour(); };
      q('.ms-op').oninput = e => { S.op = +e.target.value; save(); restyle(); };
      d.querySelectorAll('.ms-row').forEach(r => { const g = r.dataset.g, [ci, si] = r.querySelectorAll('input');
        ci.oninput = () => { S.col[g] = ci.value; save(); if (FL.includes(g) && S.by !== 'single') { S.by = 'single'; q('.ms-by').value = 'single'; } recolour(); };
        si.oninput = () => { S.sz[g] = +si.value; save(); restyle(); }; });
      q('.ms-reset').onclick = e => { e.preventDefault(); S = JSON.parse(JSON.stringify(DEF)); save(); sync(); recolour(); };
      function sync() { q('.ms-by').value = S.by; q('.ms-op').value = S.op;
        d.querySelectorAll('.ms-row').forEach(r => { const g = r.dataset.g, [ci, si] = r.querySelectorAll('input');
          ci.value = S.col[g] || (items[g] && items[g][0] && /^#[0-9a-f]{6}$/i.test(items[g][0].col) ? items[g][0].col : '#e5c07b'); si.value = sz(g);
          r.style.display = (items[g] || []).length ? '' : 'none'; }); }
      return d;
    };
    c.addTo(map);
  }
  // Shiny sends a data frame as columns; the drawing wants one object per row
  const rows = x => { if (!x || Array.isArray(x)) return x || []; const k = Object.keys(x), n = k.length ? [].concat(x[k[0]]).length : 0;
    return Array.from({ length: n }, (_, i) => Object.fromEntries(k.map(c => [c, [].concat(x[c])[i]]))); };
  const tip = s => String(s ?? '').replace(/[&<>"]/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));
  const card = (t, ...l) => `<b>${tip(t)}</b>` + l.filter(x => x !== '' && x != null).map(x => `<br><span class="mt-sub">${tip(x)}</span>`).join('');
  const pts = s => String(s || '').split(';').map(s => s.split(' ').map(Number)).filter(p => p.length === 2 && !isNaN(p[0]));
  const FP = 'Flight footprints', YF = 'Year footprints';
  const FL = [FP, 'Image positions', 'Flown tracks and receivers', 'Plans'];
  // satellite scenes, aerial photos, LiDAR tiles and other rasters: solid = footprint from the vendor's metadata,
  // dashed = extent worked out from a file header (derived)
  const GSL = ['Survey points (GNSS page)', 'GCPs and checkpoints', 'Plots, transects and grids', 'Boundaries and extents', 'Field sites and trails', 'Other GIS layers'];
  const DSL = ['Satellite footprints', 'Aerial photo coverage', 'LiDAR extents', 'Other rasters', ...GSL];
  const DSK = { satellite: DSL[0], aerial: DSL[1], lidar: DSL[2], raster: DSL[3] };
  let dsAll = [];
  function datasets(d) {
    if (typeof L === 'undefined') return; if (!map) init();
    DSL.slice(0, 4).forEach(clear); dsAll = [];
    rows(d.items).forEach(x => {
      const ll = pts(x.line); if (ll.length < 3) return; dsAll.push(...ll); const g = DSK[x.kind] || DSL[3];
      reg(g, L.polygon(ll, { pane: 'datasets', opacity: 1, dashArray: x.source ? null : '6 4' })
        .bindTooltip(card(x.label, x.source ? 'footprint from vendor metadata' : 'extent from file header'), { sticky: true }).addTo(lay[g]), { col: x.col, w: 2, fill: true });
    });
    if (d.fit && dsAll.length) map.fitBounds(dsAll, { padding: [20, 20], maxZoom: 18 });
    drawLegend();
  }
  // GIS layers and survey points: drawn from the files as they are (no editing); points as dots, lines, polygons outlined
  function shape(x, group, col) {
    const ll = pts(x.line); if (!ll.length) return; dsAll.push(...ll); const g = lay[group] ? group : 'Other GIS layers';
    const o = { pane: 'datasets', opacity: 1 }, t = card(x.label, x.layer ? x.layer : '');
    if (x.type === 'point') return reg(g, L.circleMarker(ll[0], { ...o, fillOpacity: 1 }).bindTooltip(t).addTo(lay[g]), { col, dot: true, edge: '#282c34', w: 1, r: 4 });
    reg(g, (x.type === 'polygon' ? L.polygon(ll, o) : L.polyline(ll, o)).bindTooltip(t, { sticky: true }).addTo(lay[g]), { col, w: 2, fill: x.type === 'polygon' });
  }
  function gisdraw(d) {
    if (typeof L === 'undefined') return; if (!map) init();
    GSL.slice(1).forEach(clear); rows(d.items).forEach(x => shape(x, x.category, x.col)); drawLegend();
  }
  function gnssdraw(d) {
    if (typeof L === 'undefined') return; if (!map) init();
    clear(GSL[0]); rows(d.pts).forEach(p => shape({ type: 'point', line: p.lat + ' ' + p.lon, label: p.label }, GSL[0], p.col)); drawLegend();
  }
  function pick(id) { Shiny.setInputValue('map_pick', id, { priority: 'event' }); }
  // flight colour: positioning state from the server, or a palette over year / sensor / aircraft / site, or one colour
  let fcol = {}, fkey = [], yearKey = [];
  function flightColours(fs, pal) {
    fcol = {}; fkey = [];
    if (S.by === 'status') { const m = {}; fs.forEach(f => { fcol[f.id] = f.col; m[f.status || 'unknown'] = f.col; }); fkey = Object.entries(m); return; }
    if (S.by === 'single') { const c = S.col[FP] || '#e5c07b'; fs.forEach(f => fcol[f.id] = c); fkey = [['All flights', c]]; return; }
    const v = [...new Set(fs.map(f => String(f[S.by] || 'unknown')))].sort(), m = {};
    v.forEach((x, i) => m[x] = pal[i % pal.length]); fs.forEach(f => fcol[f.id] = m[String(f[S.by] || 'unknown')]);
    fkey = v.slice(0, 12).map(x => [x, m[x]]); if (v.length > 12) fkey.push([`+ ${v.length - 12} more`, '']);
  }
  function draw(d) {
    if (typeof L === 'undefined') { document.getElementById('rsmap').innerHTML = '<p class="muted p-3">The map library could not be loaded: the map needs an internet connection (Leaflet and map tiles).</p>'; return; }
    if (!map) init(); last = d;
    FL.forEach(clear); clear(YF); boxes = {}; const all = [], fs = rows(d.flights), fp = d.fp || {};
    flightColours(fs, d.pal || ['#e5c07b']);
    // one footprint per year: the joined outlines only (each kind its own, drawn on top of each other), flights not drawn
    (d.years || []).forEach(y => {
      const parts = y.parts.map(p => p.map(pts)); parts.forEach(p => p.forEach(r => all.push(...r)));
      reg(YF, L.polygon(parts, { opacity: 1, dashArray: '2 4', lineCap: 'round' }).bindTooltip(card(`${y.year} · ${y.label}`, `${y.n} flights joined`,
          `covered ${y.area_km2} km² in ${y.n_parts} part${y.n_parts === 1 ? '' : 's'}`, `flown more than once ${y.more_km2} km²`, 'derived by ovid'), { sticky: true }).addTo(lay[YF]),
          { col: y.col, w: 2.5, fill: true });
    });
    if (d.year_mode) { yearKey = (d.years || []).map(y => [`${y.year} · ${y.label}`, y.col]); fs.length = 0; } else yearKey = [];
    fs.forEach(f => {
      if (f.lat_min === null || f.lat_min === undefined) return;
      const bb = [[f.lat_min, f.lon_min], [f.lat_max, f.lon_max]]; all.push(...bb);
      const ring = fp[f.id], t = card(f.name, [f.date, f.site].filter(Boolean).join(' · '), [f.aircraft, f.sensor].filter(Boolean).join(' · '),
                     `${f.n ?? '?'} images${f.gb ? ' · ' + f.gb + ' GB' : ''}`, f.status, ring ? `${f.fp_kind === 'lidar' ? 'LiDAR strip' : 'photo footprint'} ${f.fp_area_km2} km²` : 'no footprint: the box around its positions');
      // the ground the flight covered; a flight with none keeps the box around its positions (both derived: dotted)
      const shape = ring ? L.polygon(ring.map(p => p.map(pts)), { opacity: 1, dashArray: '2 4', lineCap: 'round' }) : L.rectangle(bb, { opacity: 1, dashArray: '2 4', lineCap: 'round' });
      boxes[f.id] = reg(FP, shape.bindTooltip(t, { sticky: true }).on('click', () => pick(f.id)).addTo(lay[FP]),
                        { col: fcol[f.id], w: f.id === sel ? 3 : 2, fill: true, sel: f.id === sel });
      boxes[f.id]._it = items[FP][items[FP].length - 1];
      (d.points[f.id] || []).forEach(p => reg('Image positions', L.circleMarker(p, { fillOpacity: .9 }).addTo(lay['Image positions']), { col: fcol[f.id], dot: true, w: 1, r: 2 }));
    });
    if (!d.year_mode) rows(d.geom).forEach(g => {
      const ll = pts(g.line); if (!ll.length) return; const plan = g.kind === 'planned', grp = plan ? 'Plans' : 'Flown tracks and receivers';
      const c = (g.id && fcol[g.id]) || '#999999', t = card(`${g.kind}: ${g.what}`, g.ref || '');
      const m = ll.length === 1 ? L.circleMarker(ll[0], { fill: false }) : L.polyline(ll, { dashArray: plan ? '8 6' : null, opacity: .95 });
      m.bindTooltip(t, { sticky: true }); if (g.id) m.on('click', () => pick(g.id));
      reg(grp, m.addTo(lay[grp]), ll.length === 1 ? { col: c, w: 2, r: 6 } : { col: c, w: plan ? 1.5 : 2.2 });
    });
    if (all.length && (!fitted || d.fit)) { map.fitBounds(all, { padding: [20, 20], maxZoom: 18 }); fitted = true; }
    drawLegend(); setTimeout(() => map.invalidateSize(), 50);
  }
  const recolour = () => { if (last) { const f = fitted; draw({ ...last, fit: false }); fitted = f; } restyle(); };
  // compact legend: flight colours, then each layer that has something on the map, with its count
  function drawLegend() {
    if (!legend || !legend.getContainer()) return; const sw = (c, dash) => `<i style="border-color:${c || 'transparent'}${dash ? ';border-style:dashed' : ''}"></i>`;
    let h = yearKey.length ? '<div class="lg-t">One footprint per year</div>' + yearKey.map(([k, c]) => `<div>${sw(c)}${tip(k)}</div>`).join('') : '';
    h += fkey.length && (items[FP] || []).length ? `<div class="lg-t">Flights by ${{ status: 'positioning', single: 'colour' }[S.by] || S.by}</div>` + fkey.map(([k, c]) => `<div>${sw(c)}${tip(k)}</div>`).join('') : '';
    const on = DSL.filter(g => (items[g] || []).length && map.hasLayer(lay[g]));
    if (on.length) h += '<div class="lg-t">Layers</div>' + on.map(g => `<div>${sw(colOf(g, items[g][0].col))}${tip(g)} <span class="mt-sub">${items[g].length}</span></div>`).join('');
    if (h) h += '<div class="mt-sub lg-n">dashed planned · solid observed · dotted derived</div>';
    legend.getContainer().innerHTML = h; legend.getContainer().style.display = h ? '' : 'none';
  }
  function select(id) {
    sel = id; if (!map) return;
    Object.entries(boxes).forEach(([k, b]) => { b._it.w = k === id ? 3 : 2; b._it.sel = k === id; style1(FP, b._it); });
    if (boxes[id]) map.fitBounds(boxes[id].getBounds(), { padding: [40, 40], maxZoom: 17 });
  }
  $(document).on('shiny:connected', () => {
    Shiny.addCustomMessageHandler('rsmap', draw);
    Shiny.addCustomMessageHandler('rsmap_select', select);
    // Report (PDF): the map as it is now, at 250 ppi with its legend, scale bar and north arrow (www/map_tools.js), back to the server
    Shiny.addCustomMessageHandler('rs-report', x => { if (!map) return Shiny.setInputValue('report_png', { error: 'the map is not drawn yet' }, { priority: 'event' });
      OvidMapTools.report(map, legend.getContainer()).then(r => Shiny.setInputValue('report_png', r, { priority: 'event' }))
        .catch(e => Shiny.setInputValue('report_png', { error: e.message }, { priority: 'event' })); });
    Shiny.addCustomMessageHandler('gnssmap', gnss);
    Shiny.addCustomMessageHandler('rsdata', datasets);
    Shiny.addCustomMessageHandler('rsgis', gisdraw);
    Shiny.addCustomMessageHandler('rsgnss', gnssdraw);
  });
  // GNSS survey points on their own map (GNSS page), Esri imagery underneath
  let gmap, glay, glegend;
  function gnss(d) {
    if (typeof L === 'undefined') return;
    if (!gmap) { gmap = L.map('gnssmap').setView([0, 0], 2);   // the world until points are drawn
      L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', { maxZoom: 21, maxNativeZoom: 19, attribution: 'Esri World Imagery' }).addTo(gmap);
      glay = L.layerGroup().addTo(gmap);
      glegend = L.control({ position: 'bottomright' }); glegend.onAdd = () => L.DomUtil.create('div', 'map-legend'); glegend.addTo(gmap);
      OvidMapTools(gmap, { legend: () => glegend.getContainer(), name: 'gnss_map', key: 'gnss' }); }
    glay.clearLayers(); const pts = rows(d.pts || []);
    pts.forEach(p => L.circleMarker([p.lat, p.lon], { radius: 5, color: '#282c34', weight: 1, fillColor: p.col, fillOpacity: 1 }).bindTooltip(p.label).addTo(glay));
    const key = Object.entries(d.key || {}), lg = glegend.getContainer();   // point colour = GNSS solution
    lg.innerHTML = key.length ? '<div class="lg-t">GNSS solution</div>' + key.map(([k, c]) => `<div><i style="border-color:${c}"></i>${tip(k)}</div>`).join('') : '';
    lg.style.display = key.length ? '' : 'none';
    if (pts.length) gmap.fitBounds(L.latLngBounds(pts.map(p => [p.lat, p.lon])).pad(0.2), { maxZoom: 18 });
  }
  $(document).on('shown.bs.tab', () => gmap && setTimeout(() => gmap.invalidateSize(), 50));
  $(document).on('shown.bs.tab', () => map && setTimeout(() => map.invalidateSize(), 50));
})();
