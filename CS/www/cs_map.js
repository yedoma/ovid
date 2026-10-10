// www/cs_map.js  --  The Station map page of Ovid Core // CS: one marker per station from Station details, coloured by whether it
// is still recording, with a legend, station names, and a Legend / PNG bar (map_tools.js). The server sends "csmap" messages.
(function () {
  let map = null, layer = null, legendEl = null, tools = null, lastFit = '';
  const esc = s => String(s == null ? '' : s).replace(/[&<>"]/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));
  function make() {
    const el = document.getElementById('cs_map'); if (!el || typeof L === 'undefined') return false;
    map = L.map(el, { zoomControl: true, attributionControl: true, preferCanvas: false }).setView([60.1, -120.1], 3)   // until the stations are drawn and fitted;
    L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
      { maxZoom: 19, maxNativeZoom: 18, crossOrigin: 'anonymous', attribution: 'Imagery &copy; Esri' }).addTo(map);
    layer = L.featureGroup().addTo(map);
    const lg = L.control({ position: 'bottomright' });
    lg.onAdd = () => { legendEl = L.DomUtil.create('div', 'cs-legend'); return legendEl; };
    lg.addTo(map);
    if (window.OvidMapTools) tools = OvidMapTools(map, { legend: () => legendEl, name: 'ovid_cs_station_map', key: 'csmap' });
    return true;
  }
  function draw(m) {
    if (!map && !make()) return;
    layer.clearLayers();
    (m.items || []).forEach(s => {
      const c = L.circleMarker([s.lat, s.lon], { radius: 7, weight: 2, color: m.edge, fillColor: s.col, fillOpacity: 0.95 });
      c.bindPopup(`<b>${esc(s.station)}</b><br>${esc(s.status)}` + (s.records ? `<br>${esc(s.records)} records, ${esc(s.first)} to ${esc(s.last)}` : '') +
        (s.loggers ? `<br>Loggers: ${esc(s.loggers)}` : '') + (s.elev ? `<br>Elevation ${esc(s.elev)} m` : '') +
        `<br>${esc(s.lat)}, ${esc(s.lon)}` + (s.desc ? `<br><span style="opacity:.8">${esc(s.desc)}</span>` : '') +
        (s.records ? `<br><a href="#" class="cs-open" data-s="${esc(s.station)}">Plot this station</a>` : ''));
      if (m.labels) c.bindTooltip(esc(s.station), { permanent: true, direction: 'right', offset: [8, 0], className: 'cs-label' });
      else c.bindTooltip(esc(s.station), { direction: 'right', offset: [8, 0] });
      layer.addLayer(c);
    });
    if (legendEl) legendEl.innerHTML = '<b>Stations</b>' + (m.legend || []).map(l =>
      `<div><span style="display:inline-block;width:11px;height:11px;border-radius:50%;background:${l.col};border:2px solid ${m.edge};margin-right:6px;vertical-align:-1px"></span>${esc(l.label)}</div>`).join('');
    if (tools) tools.refresh();
    const key = (m.items || []).map(s => s.station).join('|');
    if (key !== lastFit && layer.getLayers().length) { map.fitBounds(layer.getBounds().pad(0.15), { maxZoom: 15 }); lastFit = key; }
    setTimeout(() => map.invalidateSize(), 60);
  }
  document.addEventListener('click', e => { const a = e.target.closest && e.target.closest('a.cs-open');
    if (a) { e.preventDefault(); Shiny.setInputValue('map_pick', a.dataset.s, { priority: 'event' }); } });
  $(document).on('shiny:connected', () => {
    Shiny.addCustomMessageHandler('csmap', draw);
    Shiny.addCustomMessageHandler('csmap_resize', m => { if (map) setTimeout(() => map.invalidateSize(), 60); });
  });
})();
