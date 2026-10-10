// www/map_tools.js  --  Legend on/off and "PNG of this view" for a Leaflet map. Same file as Ovid RS www/map_tools.js; keep the two identical.
// OvidMapTools(map, { legend: () => element, name: 'map', key: 'main' }) adds a small Legend / PNG bar to a Leaflet map.
// The PNG is drawn afresh at 250 ppi (250/96 times the screen size): imagery tiles are fetched at the matching finer zoom and every
// shape on the map is redrawn, so lines stay sharp. The legend, when included, is drawn bottom right as on screen.
(function () {
  const PPI = 250, K = PPI / 96;
  const css = `.dmt{font-size:12.5px}.dmt a{width:auto!important;padding:0 9px;font-weight:600;cursor:pointer}.dmt a.off{opacity:.55;text-decoration:line-through}
.dmt-menu{background:var(--bg1,var(--panel,#383c44));color:var(--fg,#bcbfc4);padding:4px 0;border-top:1px solid rgba(128,128,128,.4)}
.dmt-menu a{display:block;height:auto!important;line-height:1.8;text-align:left;font-weight:400;border:0!important;background:none!important;color:inherit!important}
.dmt-menu a:hover{text-decoration:underline}.dmt-hidden{display:none!important}`;
  function addCss() { if (document.getElementById('dmt-css')) return; const s = document.createElement('style'); s.id = 'dmt-css'; s.textContent = css; document.head.appendChild(s); }
  const store = { get(k) { try { return localStorage.getItem('ovidrs.legend.' + k); } catch (e) { return null; } },
                  set(k, v) { try { localStorage.setItem('ovidrs.legend.' + k, v); } catch (e) {} } };

  window.OvidMapTools = function (map, o) {
    addCss(); const key = o.key || 'map', leg = () => o.legend && o.legend();
    let shown = store.get(key) !== 'off';
    const apply = () => { const el = leg(); if (el) el.classList.toggle('dmt-hidden', !shown); if (btn) btn.classList.toggle('off', !shown); };
    let btn;
    const c = L.control({ position: o.position || 'topleft' });
    c.onAdd = () => {
      const d = L.DomUtil.create('div', 'dmt leaflet-bar'); L.DomEvent.disableClickPropagation(d); L.DomEvent.disableScrollPropagation(d);
      d.innerHTML = `<a class="dmt-leg" title="Show or hide the legend">Legend</a><a class="dmt-png" title="Save this view as a ${PPI} ppi PNG">PNG</a>
        <div class="dmt-menu" hidden><a data-l="1">PNG with legend</a><a data-l="0">PNG without legend</a></div>`;
      btn = d.querySelector('.dmt-leg'); const menu = d.querySelector('.dmt-menu');
      btn.onclick = e => { e.preventDefault(); shown = !shown; store.set(key, shown ? 'on' : 'off'); apply(); };
      d.querySelector('.dmt-png').onclick = e => { e.preventDefault(); menu.hidden = !menu.hidden; };
      menu.querySelectorAll('a').forEach(a => a.onclick = e => { e.preventDefault(); menu.hidden = true;
        exportPng(map, a.dataset.l === '1' ? leg() : null, o.name || 'map').catch(err => alert('The PNG could not be made: ' + err.message)); });
      return d;
    };
    c.addTo(map); apply();
    return { refresh: apply };   // call after the legend is redrawn, so a hidden legend stays hidden
  };

  // ---- drawing ----
  function rings(ll) { if (!Array.isArray(ll)) return []; if (!ll.length) return []; if (ll[0] instanceof L.LatLng || !Array.isArray(ll[0])) return [ll]; return ll.flatMap(rings); }
  function loadImg(src) { return new Promise(res => { const i = new Image(); i.crossOrigin = 'anonymous'; i.onload = () => res(i); i.onerror = () => res(null); i.src = src; }); }

  async function exportPng(map, legendEl, name) {
    const size = map.getSize(), W = Math.round(size.x * K), H = Math.round(size.y * K);
    const cv = document.createElement('canvas'); cv.width = W; cv.height = H; const g = cv.getContext('2d');
    g.fillStyle = '#282c34'; g.fillRect(0, 0, W, H);
    const z = map.getZoom(), nw = map.containerPointToLatLng([0, 0]);
    // imagery: tiles at the zoom that matches 250 ppi (capped at the finest the service has)
    const tls = []; map.eachLayer(l => { if (l instanceof L.TileLayer) tls.push(l); });
    for (const t of tls) {
      const zmax = t.options.maxNativeZoom ?? t.options.maxZoom ?? 19, zt = Math.max(0, Math.min(zmax, Math.round(z + Math.log2(K))));
      const f = K * Math.pow(2, z - zt), o = map.project(nw, zt), ts = t.getTileSize().x, n = Math.pow(2, zt);
      const x0 = Math.floor(o.x / ts), y0 = Math.floor(o.y / ts), x1 = Math.floor((o.x + W / f) / ts), y1 = Math.floor((o.y + H / f) / ts), jobs = [];
      for (let x = x0; x <= x1; x++) for (let y = Math.max(0, y0); y <= Math.min(n - 1, y1); y++) {
        const url = L.Util.template(t._url, L.extend({}, t.options, { x: ((x % n) + n) % n, y, z: zt, s: 'a', r: '' }));
        jobs.push(loadImg(url).then(im => im && g.drawImage(im, (x * ts - o.x) * f, (y * ts - o.y) * f, ts * f + 0.5, ts * f + 0.5)));
      }
      await Promise.all(jobs);
    }
    // shapes, in pane order (datasets under flights), as styled on screen
    const vs = []; map.eachLayer(l => { if (l instanceof L.Path) vs.push(l); });
    const zi = l => +(map.getPane(l.options.pane || 'overlayPane').style.zIndex || 400);
    vs.sort((a, b) => zi(a) - zi(b) || L.stamp(a) - L.stamp(b));
    const P = ll => { const p = map.latLngToContainerPoint(ll); return [p.x * K, p.y * K]; };
    for (const l of vs) {
      const s = l.options; if (s.stroke === false && !s.fill) continue;
      g.beginPath();
      if (l instanceof L.CircleMarker) { const [x, y] = P(l.getLatLng()); g.arc(x, y, (l.getRadius ? l.getRadius() : s.radius) * K, 0, 2 * Math.PI); }
      else { const closed = l instanceof L.Polygon;
        rings(l.getLatLngs()).forEach(r => { r.forEach((ll, i) => { const [x, y] = P(ll); i ? g.lineTo(x, y) : g.moveTo(x, y); }); if (closed) g.closePath(); }); }
      if (s.fill) { g.globalAlpha = s.fillOpacity ?? 0.2; g.fillStyle = s.fillColor || s.color; g.fill('evenodd'); }
      if (s.stroke !== false) { g.globalAlpha = s.opacity ?? 1; g.strokeStyle = s.color; g.lineWidth = (s.weight ?? 3) * K; g.lineCap = s.lineCap || 'round'; g.lineJoin = s.lineJoin || 'round';
        g.setLineDash(s.dashArray ? String(s.dashArray).split(/[ ,]+/).map(v => +v * K) : []); g.stroke(); g.setLineDash([]); }
      g.globalAlpha = 1;
    }
    // attribution, bottom left (the imagery licence asks for it)
    const att = tls.map(t => t.options.attribution).filter(Boolean).join(' · ');
    if (att) { g.font = `${11 * K}px system-ui, sans-serif`; const w = g.measureText(att).width + 10 * K;
      g.fillStyle = 'rgba(40,44,52,.75)'; g.fillRect(0, H - 17 * K, w, 17 * K); g.fillStyle = '#e6e6e6'; g.textBaseline = 'middle'; g.fillText(att, 5 * K, H - 8.5 * K); }
    if (legendEl && legendEl.textContent.trim()) drawLegend(g, legendEl, W, H);
    const blob = await new Promise(r => cv.toBlob(r, 'image/png'));
    const png = withPpi(new Uint8Array(await blob.arrayBuffer()));
    const a = document.createElement('a'); a.href = URL.createObjectURL(new Blob([png], { type: 'image/png' }));
    a.download = `ovid_rs_${name}_${new Date().toISOString().slice(0, 10)}.png`; document.body.appendChild(a); a.click();
    setTimeout(() => { URL.revokeObjectURL(a.href); a.remove(); }, 1000);
  }

  // the legend as on screen: one line per row, a swatch where the row has one, bold headings, muted notes
  function drawLegend(g, el, W, H) {
    const was = el.classList.contains('dmt-hidden'); if (was) el.classList.remove('dmt-hidden');
    const cs = getComputedStyle(el), rows = [...el.children].filter(r => r.textContent.trim());
    const fs = 12 * K, lh = fs * 1.5, pad = 9 * K, sw = 20 * K;
    const font = r => `${+getComputedStyle(r).fontWeight >= 600 ? '600 ' : ''}${fs * (r.classList.contains('lg-n') ? 0.92 : 1)}px system-ui, sans-serif`;
    const items = rows.map(r => { const i = r.querySelector('i'), ic = i && getComputedStyle(i);
      return { t: r.textContent.trim().replace(/\s+/g, ' '), f: font(r), c: getComputedStyle(r).color,
               sw: i ? { c: ic.borderTopColor, d: ic.borderTopStyle === 'dashed' } : null, muted: r.classList.contains('lg-n') }; });
    if (was) el.classList.add('dmt-hidden');
    let w = 0; items.forEach(it => { g.font = it.f; w = Math.max(w, g.measureText(it.t).width + (it.sw ? sw : 0)); });
    const bw = w + 2 * pad, bh = items.length * lh + 2 * pad, x0 = W - bw - 10 * K, y0 = H - bh - 22 * K;
    g.fillStyle = cs.backgroundColor && cs.backgroundColor !== 'rgba(0, 0, 0, 0)' ? cs.backgroundColor : '#383c44';
    g.strokeStyle = cs.borderTopColor || '#484c54'; g.lineWidth = K;
    g.beginPath(); g.roundRect ? g.roundRect(x0, y0, bw, bh, 6 * K) : g.rect(x0, y0, bw, bh); g.fill(); g.stroke();
    g.textBaseline = 'middle';
    items.forEach((it, k) => { const y = y0 + pad + lh * (k + 0.5); let x = x0 + pad;
      if (it.sw) { g.strokeStyle = it.sw.c; g.lineWidth = 3 * K; g.setLineDash(it.sw.d ? [4 * K, 3 * K] : []); g.beginPath(); g.moveTo(x, y); g.lineTo(x + 14 * K, y); g.stroke(); g.setLineDash([]); x += sw; }
      g.font = it.f; g.fillStyle = it.muted ? '#999999' : it.c; g.fillText(it.t, x, y); });
  }

  // PNG files carry their print size in a pHYs chunk: 250 ppi = 9843 pixels per metre
  const CRC = (() => { const t = new Uint32Array(256); for (let n = 0; n < 256; n++) { let c = n; for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1; t[n] = c >>> 0; } return t; })();
  const crc = b => { let c = 0xffffffff; for (const x of b) c = CRC[(c ^ x) & 255] ^ (c >>> 8); return (c ^ 0xffffffff) >>> 0; };
  function withPpi(png) {
    const ppm = Math.round(PPI / 0.0254), ch = new Uint8Array(21), v = new DataView(ch.buffer);
    v.setUint32(0, 9); ch.set([112, 72, 89, 115], 4); v.setUint32(8, ppm); v.setUint32(12, ppm); ch[16] = 1; v.setUint32(17, crc(ch.subarray(4, 17)));
    const out = new Uint8Array(png.length + 21); out.set(png.subarray(0, 33)); out.set(ch, 33); out.set(png.subarray(33), 54); return out;   // right after IHDR
  }
})();
