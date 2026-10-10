
$(document).on('click', '[data-wb-input]', function(e){ e.preventDefault();
  Shiny.setInputValue(this.getAttribute('data-wb-input'), this.getAttribute('data-wb-key'), {priority: 'event'}); });
$(document).on('shown.bs.tab', function(){ if ($.fn.dataTable) setTimeout(function(){ $($.fn.dataTable.tables(true)).DataTable().columns.adjust(); }, 30); });
// pages: the inspector shows beside the Workbench views (values wb-...), app pages get the full width; the tree marks the page
function wbNav(v){ var b = document.querySelector('.wb-body'); if (b) b.classList.toggle('wide', !!v && !/^wb-(data|import)$/.test(v)); if (window.wbPanels) wbPanels.apply();
  document.querySelectorAll('.wb-nav [data-ovid-go]').forEach(function(a){ a.classList.toggle('on', a.getAttribute('data-ovid-go') === v); }); }
$(document).on('shiny:inputchanged', function(e){ if (e.name === 'nav') wbNav(e.value); });
$(document).on('shown.bs.tab', '#nav a', function(e){ wbNav(e.target.getAttribute('data-value')); });
// open a page, then a tab or a details section on it: {go, tab, details}
function wbOpen(m){ if (window.ovidGo) window.ovidGo(m.go);
  setTimeout(function(){ var pane = document.querySelector('.tab-pane[data-value="' + CSS.escape(m.go) + '"]'); if (!pane) return;
    if (m.tab) { var a = pane.querySelector('a[data-value="' + CSS.escape(m.tab) + '"]'); if (a) a.click(); }
    if (m.details) pane.querySelectorAll('details').forEach(function(d){ var s = d.querySelector('summary');
      if (s && s.textContent.indexOf(m.details) >= 0) { d.open = true; setTimeout(function(){ d.scrollIntoView({block: 'start'}); }, 50); } }); }, 80); }
$(document).on('click', '.wb-needs a[data-ovid-go]', function(){ var d = this.closest('details'); if (d) d.open = false; });
$(document).on('click', '[data-wb-open]', function(e){ e.preventDefault(); try { wbOpen(JSON.parse(this.getAttribute('data-wb-open'))); } catch (x) {} });
// the grid's tools: Find marks matching cells, Go to date moves the period, Copy (or Cmd/Ctrl+C) gives the selected cells as TSV
$(document).on('input', '.wb-gfind', function(){ var q = this.value.trim().toLowerCase(), n = 0, first = null;
  document.querySelectorAll('.wb-gt tbody td').forEach(function(td){ var hit = !!q && td.textContent.toLowerCase().indexOf(q) >= 0; td.classList.toggle('hit', hit); if (hit) { n++; if (!first) first = td; } });
  var m = this.parentNode.querySelector('.wb-gmsg'); if (m) m.textContent = q ? (n ? n + ' cell' + (n > 1 ? 's' : '') + ' found' : 'not in the rows shown: widen the period') : '';
  if (first) first.scrollIntoView({block: 'center', inline: 'nearest'}); });
$(document).on('change', '.wb-ggoto', function(){ if (this.value) Shiny.setInputValue('wb_goto', this.value, {priority: 'event'}); });
function wbGridCopy(){ var rows = {}; document.querySelectorAll('.wb-gt td.sel').forEach(function(td){ var tr = td.parentNode, r = tr.getAttribute('data-r');
    (rows[r] = rows[r] || {t: (tr.querySelector('th.rh') || {}).textContent || '', c: []}).c.push(td.textContent); });
  var keys = Object.keys(rows).sort(function(a, b){ return a - b; }); if (!keys.length) return 0;
  var txt = keys.map(function(k){ return [rows[k].t].concat(rows[k].c).join('\t'); }).join('\n');
  if (navigator.clipboard && navigator.clipboard.writeText) navigator.clipboard.writeText(txt).catch(function(){}); return keys.length; }
$(document).on('click', '[data-wb-gcopy]', function(){ var n = wbGridCopy(), m = this.parentNode.querySelector('.wb-gmsg'); if (m) m.textContent = n ? 'copied ' + n + ' row' + (n > 1 ? 's' : '') : 'select cells first'; });
$(document).on('keydown', '.wb-gwrap', function(e){ if ((e.metaKey || e.ctrlKey) && e.key.toLowerCase() === 'c') { if (wbGridCopy()) e.preventDefault(); } });
// the grid's columns: sort, filters, resize (double-click fits), reorder by the grip; widths and order kept per app and table
(function(){ var APP = (document.querySelector('.wb-brand .app') || {}).textContent || 'app', KEY = function(k){ return 'ovid.grid.' + APP + '.' + k; };
  function get(k, d){ try { var v = localStorage.getItem(KEY(k)); return v ? JSON.parse(v) : d; } catch (x) { return d; } }
  function put(k, v){ try { localStorage.setItem(KEY(k), JSON.stringify(v)); } catch (x) {} }
  function tbl(){ return document.querySelector('table.wb-gt'); }
  function colTh(t, c){ return t.querySelector('th.ch[data-c=\"' + c + '\"]'); }
  function idx(t, th){ return [].indexOf.call(th.parentNode.children, th); }
  function apply(){ var t = tbl(); if (!t || t._wbx) return; t._wbx = true; var w = get('widths', {}), order = get('order', null);
    t.querySelectorAll('th.ch').forEach(function(th){ var p = th.getAttribute('data-p'); if (w[p]) th.style.width = th.style.minWidth = w[p] + 'px'; });
    if (order && order.length) { var hr = t.tHead.rows[0], ths = [].slice.call(hr.querySelectorAll('th.ch')), byP = {}; ths.forEach(function(th){ byP[th.getAttribute('data-p')] = th; });
      order.forEach(function(p){ var th = byP[p]; if (th) move(t, th, null); }); }
    look(t); }
  // show or hide columns, decimals per column, wrap: by column name, kept per app; display only
  function pmap(t){ var m = {}; t.querySelectorAll('th.ch').forEach(function(th){ m[th.getAttribute('data-p')] = th.getAttribute('data-c'); }); return m; }
  function look(t){ t = t || tbl(); if (!t) return; var m = pmap(t), hid = get('hidden', []), fmt = get('fmt', {}), css = [];
    hid.forEach(function(p){ if (m[p]) css.push('table.wb-gt [data-c="' + m[p] + '"]{display:none}'); });
    var st = document.getElementById('wb-ghide'); if (!st) { st = document.createElement('style'); st.id = 'wb-ghide'; document.head.appendChild(st); } st.textContent = css.join('');
    Object.keys(m).forEach(function(p){ var d = fmt[p]; t.querySelectorAll('td[data-c="' + m[p] + '"]').forEach(function(td){ fmtCell(td, d); }); });
    t.classList.toggle('wb-wrap', !!get('wrap', false)); var wb = document.querySelector('[data-wb-gwrap]'); if (wb) wb.classList.toggle('on', !!get('wrap', false)); }
  function fmtCell(td, d){ var v = td.getAttribute('data-v'); if (td._o === undefined) { if (d === undefined || d === null || d === '') return; td._o = td.innerHTML; }
    td.innerHTML = td._o; if (d === undefined || d === null || d === '' || v === null) return;
    var w = document.createTreeWalker(td, NodeFilter.SHOW_TEXT), n, rx = /[\u2212-]?\d[\d,]*\.?\d*(e[\u2212+-]?\d+)?/i;
    while ((n = w.nextNode())) { if (rx.test(n.nodeValue)) { var neg = n.nodeValue.indexOf('\u2212') >= 0, x = (+v).toFixed(+d); if (neg) x = x.replace('-', '\u2212'); n.nodeValue = n.nodeValue.replace(rx, x); break; } } }
  $(document).on('click', '[data-wb-gcols]', function(e){ e.stopPropagation(); var w = this.parentNode, pop = w.querySelector('.wb-gcols-pop'); if (pop) return pop.remove();
    var t = tbl(); if (!t) return; var hid = get('hidden', []), fmt = get('fmt', {}); pop = document.createElement('div'); pop.className = 'wb-gcols-pop';
    pop.innerHTML = [].map.call(t.querySelectorAll('th.ch'), function(th){ var p = th.getAttribute('data-p'), c = th.getAttribute('data-c'), num = !!t.querySelector('td[data-c="' + c + '"][data-v]'), q = p.replace(/"/g, '&quot;');
      return '<label><input type="checkbox" data-p="' + q + '"' + (hid.indexOf(p) < 0 ? ' checked' : '') + '><span>' + q + '</span>' + (num ? '<select data-f="' + q + '" title="Decimals shown"><option value="">as is</option>' +
        [0, 1, 2, 3, 4].map(function(d){ return '<option value="' + d + '"' + (String(fmt[p]) === String(d) ? ' selected' : '') + '>' + d + ' dp</option>'; }).join('') + '</select>' : '') + '</label>'; }).join('');
    pop.addEventListener('click', function(ev){ ev.stopPropagation(); });
    pop.addEventListener('change', function(ev){ var x = ev.target;
      if (x.hasAttribute('data-f')) { var f = get('fmt', {}); if (x.value === '') delete f[x.getAttribute('data-f')]; else f[x.getAttribute('data-f')] = +x.value; put('fmt', f); }
      else { var h = get('hidden', []), p = x.getAttribute('data-p'); h = h.filter(function(y){ return y !== p; }); if (!x.checked) h.push(p);
        if (h.length >= t.querySelectorAll('th.ch').length) { x.checked = true; return; } put('hidden', h); }
      look(); });
    w.appendChild(pop); });
  $(document).on('click', function(e){ if ($(e.target).closest('.wb-gcolw').length) return; document.querySelectorAll('.wb-gcols-pop').forEach(function(x){ x.remove(); }); });
  $(document).on('click', '[data-wb-gwrap]', function(){ put('wrap', !get('wrap', false)); look(); });
  function move(t, th, before){ var from = idx(t, th), to = before ? idx(t, before) : null;
    [].forEach.call(t.rows, function(tr){ var cell = tr.children[from]; if (!cell) return; if (to === null) tr.appendChild(cell); else tr.insertBefore(cell, tr.children[to > from ? to : to]); }); }
  $(document).on('shiny:value', function(e){ if (e.name === 'wb_grid') setTimeout(apply, 0); });
  // sort: by a column (up, down), then back to time
  $(document).on('mousedown click', '.wb-sort', function(e){ e.stopPropagation(); if (e.type === 'mousedown') return e.preventDefault();
    var t = tbl(), th = this.closest('th'), c = th.getAttribute('data-c'), st = this.getAttribute('data-s') || ''; st = st === '' ? 'up' : st === 'up' ? 'down' : '';
    t.querySelectorAll('.wb-sort').forEach(function(x){ x.classList.remove('on'); x.textContent = '\u21c5'; x.removeAttribute('data-s'); });
    if (st) { this.classList.add('on'); this.setAttribute('data-s', st); this.textContent = st === 'up' ? '\u25b2' : '\u25bc'; }
    var body = t.tBodies[0], rows = [].slice.call(body.rows), val = function(tr){ if (!st) return +tr.getAttribute('data-r'); var td = tr.querySelector('td[data-c=\"' + c + '\"]'), v = td && td.getAttribute('data-v'); return v === null || v === undefined ? NaN : +v; };
    rows.sort(function(a, b){ var x = val(a), y = val(b); if (isNaN(x) && isNaN(y)) return 0; if (isNaN(x)) return 1; if (isNaN(y)) return -1; return st === 'down' ? y - x : x - y; });
    rows.forEach(function(r){ body.appendChild(r); }); });
  // filters: >5, <=0, 1..3, =2, or text the cell holds
  function test(q, td){ var v = td ? td.getAttribute('data-v') : null, x = v === null ? NaN : +v, m; q = q.trim(); if (!q) return true;
    if ((m = /^(-?[\d.]+)\s*\.\.\s*(-?[\d.]+)$/.exec(q))) return x >= +m[1] && x <= +m[2];
    if ((m = /^(>=|<=|!=|>|<|=)\s*(-?[\d.]+)$/.exec(q))) { var y = +m[2]; return m[1] === '>' ? x > y : m[1] === '<' ? x < y : m[1] === '>=' ? x >= y : m[1] === '<=' ? x <= y : m[1] === '=' ? x === y : x !== y; }
    return ((td && td.textContent) || '').toLowerCase().indexOf(q.toLowerCase()) >= 0; }
  $(document).on('input', '.wb-gfilt', function(){ var t = tbl(), fs = [].slice.call(t.querySelectorAll('.wb-gfilt')).filter(function(i){ return i.value.trim(); }), shown = 0;
    [].forEach.call(t.tBodies[0].rows, function(tr){ var ok = fs.every(function(i){ return test(i.value, tr.querySelector('td[data-c=\"' + i.getAttribute('data-c') + '\"]')); }); tr.classList.toggle('wb-hide', !ok); if (ok) shown++; });
    var m = document.querySelector('.wb-gmsg'); if (m) m.textContent = fs.length ? shown + ' of ' + t.tBodies[0].rows.length + ' rows match the filters' : ''; });
  $(document).on('mousedown', '.wb-gfilt, .wb-gfrow th', function(e){ e.stopPropagation(); });
  // resize by the header's right edge; double-click fits the column to its widest cell
  $(document).on('mousedown', '.wb-rsz', function(e){ e.preventDefault(); e.stopPropagation(); var th = this.closest('th'), x0 = e.clientX, w0 = th.getBoundingClientRect().width;
    function mv(ev){ var w = Math.max(40, w0 + ev.clientX - x0); th.style.width = th.style.minWidth = w + 'px'; }
    function up(){ $(document).off('mousemove', mv).off('mouseup', up); var w = get('widths', {}); w[th.getAttribute('data-p')] = Math.round(th.getBoundingClientRect().width); put('widths', w); }
    $(document).on('mousemove', mv).on('mouseup', up); });
  $(document).on('dblclick', '.wb-rsz', function(e){ e.stopPropagation(); var th = this.closest('th'), t = tbl(), c = th.getAttribute('data-c'), mx = 40;
    th.style.width = th.style.minWidth = ''; var cx = document.createElement('canvas').getContext('2d'), tw = function(x){ cx.font = getComputedStyle(x).font; return cx.measureText(x.textContent).width; };
    t.querySelectorAll('td[data-c=\"' + c + '\"]').forEach(function(td){ mx = Math.max(mx, tw(td) + 16); }); mx = Math.round(Math.min(600, Math.max(mx, tw(th) + 30)));
    th.style.width = th.style.minWidth = mx + 'px'; var w = get('widths', {}); w[th.getAttribute('data-p')] = mx; put('widths', w); });
  // reorder: drag a column by its grip onto another
  var dragTh = null;
  $(document).on('dragstart', '.wb-grip', function(e){ dragTh = this.closest('th'); e.originalEvent.dataTransfer.effectAllowed = 'move'; try { e.originalEvent.dataTransfer.setData('text/plain', 'col'); } catch (x) {} });
  $(document).on('dragover', 'table.wb-gt th.ch', function(e){ if (!dragTh) return; e.preventDefault(); document.querySelectorAll('th.ch.dropl').forEach(function(x){ x.classList.remove('dropl'); }); this.classList.add('dropl'); });
  $(document).on('drop', 'table.wb-gt th.ch', function(e){ if (!dragTh) return; e.preventDefault(); var t = tbl(), to = this; this.classList.remove('dropl'); if (to !== dragTh) move(t, dragTh, to);
    put('order', [].slice.call(t.tHead.rows[0].querySelectorAll('th.ch')).map(function(th){ return th.getAttribute('data-p'); })); dragTh = null; });
  $(document).on('dragend', '.wb-grip', function(){ dragTh = null; document.querySelectorAll('th.ch.dropl').forEach(function(x){ x.classList.remove('dropl'); }); });
  // the bar between the chart and the table: drag to share the room, kept per app; both rescale as it moves
  $(document).on('mousedown', '.wb-split', function(e){ e.preventDefault(); var ch = document.querySelector('#wb_chart, #wb_xy'); if (!ch) return; var y0 = e.clientY, h0 = ch.getBoundingClientRect().height;
    function mv(ev){ ch.style.height = Math.max(120, Math.min(window.innerHeight - 220, h0 + ev.clientY - y0)) + 'px'; }
    function up(){ $(document).off('mousemove', mv).off('mouseup', up); put('chart_h', Math.round(ch.getBoundingClientRect().height)); }
    $(document).on('mousemove', mv).on('mouseup', up); });
  $(document).on('dblclick', '.wb-split', function(){ var ch = document.querySelector('#wb_chart, #wb_xy'); if (ch) ch.style.height = ''; put('chart_h', null); });
  $(function(){ var h = get('chart_h', null); if (h) setTimeout(function(){ var ch = document.querySelector('#wb_chart, #wb_xy'); if (ch) ch.style.height = h + 'px'; }, 300); });
})();
// the grid (wb_grid): selection kept as "row:col" keys, sent as runs of rows per column
(function(){
  var G = {sel: new Set(), base: new Set(), anchor: null, active: null, drag: null, sig: null, table: null, cells: {}, rh: {}, ch: {}, nr: 0, nc: 0, input: null, timer: null, painted: new Set()};
  function K(r, c){ return r + ':' + c; }
  function rect(a, b){ var s = new Set(), r0 = Math.min(a.r, b.r), r1 = Math.max(a.r, b.r), c0 = Math.min(a.c, b.c), c1 = Math.max(a.c, b.c);
    for (var r = r0; r <= r1; r++) for (var c = c0; c <= c1; c++) s.add(K(r, c)); return s; }
  function union(a, b){ var s = new Set(a); b.forEach(function(x){ s.add(x); }); return s; }
  function rows(a, b){ return rect({r: a, c: 1}, {r: b, c: G.nc}); }
  function cols(a, b){ return rect({r: 1, c: a}, {r: G.nr, c: b}); }
  function index(t){ G.table = t; G.cells = {}; G.rh = {}; G.ch = {}; G.painted = new Set(); G.times = [0]; G.params = [''];
    G.nr = +t.getAttribute('data-nr'); G.nc = +t.getAttribute('data-nc'); G.input = t.getAttribute('data-input'); G.rowOf = {};
    t.querySelectorAll('tbody tr').forEach(function(tr){ var r = tr.getAttribute('data-r'); G.rh[r] = tr.firstChild; G.times[+r] = +tr.getAttribute('data-t'); G.rowOf[G.times[+r]] = +r;
      tr.querySelectorAll('td').forEach(function(td){ G.cells[K(r, td.getAttribute('data-c'))] = td; }); });
    t.querySelectorAll('th.ch').forEach(function(th){ G.ch[th.getAttribute('data-c')] = th; G.params[+th.getAttribute('data-c')] = th.getAttribute('data-p'); }); }
  // tell the chart what is selected: [{t, p, v, r}] and the active cell
  var emitting = false;
  function emit(){ if (emitting) return; emitting = true; requestAnimationFrame(function(){ emitting = false; var cells = [];
    G.sel.forEach(function(k){ var q = k.split(':'), r = +q[0], c = +q[1], td = G.cells[k]; if (!G.times[r] || !G.params[c]) return;
      var v = td && td.hasAttribute('data-v') ? +td.getAttribute('data-v') : null; cells.push({t: G.times[r], p: G.params[c], v: v, r: r}); });
    var a = G.active && G.times[G.active.r] ? {t: G.times[G.active.r], p: G.params[G.active.c]} : null;
    document.dispatchEvent(new CustomEvent('wb-grid-sel', {detail: {cells: cells, active: a}})); }); }
  function rowAt(t){ var lo = 1, hi = G.nr; if (!hi) return 0; while (lo < hi) { var m = (lo + hi) >> 1; if (G.times[m] < t) lo = m + 1; else hi = m; }
    if (lo > 1 && Math.abs(G.times[lo - 1] - t) < Math.abs(G.times[lo] - t)) lo--; return lo; }
  function colOf(p){ for (var c = 1; c <= G.nc; c++) if (G.params[c] === p) return c; return 0; }
  function show(r, c){ var td = G.cells[K(r, c)]; if (td) td.scrollIntoView({block: 'center', inline: 'nearest'}); }
  // the chart selects: the records of parameter p from t0 to t1, or the record nearest t; add keeps the selection
  window.wbGridSelectRange = function(p, t0, t1, add){ var c = colOf(p); if (!c || !G.nr) return 0; var keys = new Set(), first = 0, last = 0;
    for (var r = 1; r <= G.nr; r++) if (G.times[r] >= t0 && G.times[r] <= t1) { keys.add(K(r, c)); if (!first) first = r; last = r; }
    if (!keys.size) return 0; G.base = add ? new Set(G.sel) : new Set(); G.sel = union(G.base, keys); G.anchor = {r: first, c: c}; G.active = {r: last, c: c};
    paint(); show(first, c); later(); return keys.size; };
  window.wbGridSelectNear = function(p, t, add){ var c = colOf(p); if (!c || !G.nr) return 0; var r = rowAt(t), k = K(r, c);
    if (add) { G.base = new Set(G.sel); if (G.sel.has(k)) { G.base.delete(k); G.sel = new Set(G.base); } else G.sel = union(G.base, new Set([k])); }
    else { G.base = new Set(); G.sel = new Set([k]); }
    G.anchor = {r: r, c: c}; G.active = {r: r, c: c}; paint(); show(r, c); later(); return 1; };
  window.wbGridClear = function(){ if (!G.sel || !G.sel.size) return; G.sel = new Set(); G.base = new Set(); G.active = null; paint(); later(); };
  window.wbGridScrollTo = function(t){ if (!G.nr) return; var r = rowAt(t); var th = G.rh[r]; var w = G.table && G.table.closest('.wb-gwrap');
    if (th && w) w.scrollTop = Math.max(0, th.parentNode.offsetTop - (G.table.tHead ? G.table.tHead.offsetHeight : 0)); };
  // a chart selects whole records by their keys (each row's data-t): add keeps the selection, and a cmd/ctrl-click on records
  // already selected takes them out
  window.wbGridSelectRows = function(ts, add){ if (!G.nr) return 0; var keys = new Set(), rs = [];
    (ts || []).forEach(function(t){ var r = G.rowOf[t]; if (!r) return; rs.push(r); for (var c = 1; c <= G.nc; c++) keys.add(K(r, c)); });
    if (!rs.length) { if (!add) { G.sel = new Set(); G.base = new Set(); G.active = null; paint(); later(); } return 0; }
    rs.sort(function(a, b){ return a - b; }); var all = true; keys.forEach(function(k){ if (!G.sel.has(k)) all = false; });
    if (add && all) { G.base = new Set(G.sel); keys.forEach(function(k){ G.base.delete(k); }); G.sel = new Set(G.base); }
    else { G.base = add ? new Set(G.sel) : new Set(); G.sel = union(G.base, keys); }
    G.anchor = {r: rs[0], c: 1}; G.active = {r: rs[rs.length - 1], c: 1}; paint(); show(rs[0], 1); later(); return rs.length; };
  function paint(){
    G.painted.forEach(function(k){ if (!G.sel.has(k)) { var td = G.cells[k]; if (td) td.classList.remove('sel'); } });
    G.sel.forEach(function(k){ if (!G.painted.has(k)) { var td = G.cells[k]; if (td) td.classList.add('sel'); } });
    G.painted = new Set(G.sel);
    if (G.table) { G.table.querySelectorAll('td.act').forEach(function(x){ x.classList.remove('act'); });
      G.table.querySelectorAll('th.sel').forEach(function(x){ x.classList.remove('sel'); }); }
    if (G.active) { var a = G.cells[K(G.active.r, G.active.c)]; if (a) a.classList.add('act'); }
    var nr = {}, nc = {}; G.sel.forEach(function(k){ var p = k.split(':'); nr[p[0]] = (nr[p[0]] || 0) + 1; nc[p[1]] = (nc[p[1]] || 0) + 1; });
    Object.keys(nr).forEach(function(r){ if (nr[r] === G.nc && G.rh[r]) G.rh[r].classList.add('sel'); });
    Object.keys(nc).forEach(function(c){ if (nc[c] === G.nr && G.ch[c]) G.ch[c].classList.add('sel'); }); emit(); stats(); }
  // count, sum, mean, min and max of the selected cells that hold a number (hidden columns left out), as Excel's status bar
  function stats(){ var el = document.querySelector('.wb-gstat'); if (!el) return; var n = 0, s = 0, lo = Infinity, hi = -Infinity, all = 0;
    G.sel.forEach(function(k){ var td = G.cells[k]; if (!td || td.offsetParent === null) return; all++; var v = td.getAttribute('data-v'); if (v === null) return; v = +v; if (!isFinite(v)) return;
      n++; s += v; if (v < lo) lo = v; if (v > hi) hi = v; });
    var f = function(x){ return String(+x.toPrecision(4)).replace('-', '\u2212'); };
    el.textContent = all < 2 ? '' : n ? 'count ' + n + ' \u00b7 sum ' + f(s) + ' \u00b7 mean ' + f(s / n) + ' \u00b7 min ' + f(lo) + ' \u00b7 max ' + f(hi) : all + ' cells, no numbers'; }
  function send(){ if (!G.input || !window.Shiny || !Shiny.setInputValue) return;
    var by = {}; G.sel.forEach(function(k){ var p = k.split(':'); (by[p[1]] = by[p[1]] || []).push(+p[0]); });
    var c = [], r1 = [], r2 = [];
    Object.keys(by).forEach(function(cc){ var rs = by[cc].sort(function(a, b){ return a - b; }), s = rs[0], e = rs[0];
      for (var i = 1; i <= rs.length; i++) { if (i < rs.length && rs[i] === e + 1) { e = rs[i]; continue; }
        c.push(+cc); r1.push(s); r2.push(e); if (i < rs.length) { s = rs[i]; e = rs[i]; } } });
    Shiny.setInputValue(G.input, {sig: G.sig, c: c, r1: r1, r2: r2, ar: G.active ? G.active.r : 0, ac: G.active ? G.active.c : 0, n: G.sel.size}); }
  function later(){ clearTimeout(G.timer); G.timer = setTimeout(send, 90); }
  function focus(){ var w = G.table && G.table.closest('.wb-gwrap'); if (w) w.focus({preventScroll: true}); }
  function pick(next, e, anchorTo){ if (e.shiftKey && G.anchor) G.sel = union(G.base, next(G.anchor));
    else if (e.metaKey || e.ctrlKey) { G.base = new Set(G.sel); var all = true; next(anchorTo).forEach(function(k){ if (!G.sel.has(k)) all = false; });
      if (all) { next(anchorTo).forEach(function(k){ G.base.delete(k); }); G.sel = new Set(G.base); G.anchor = anchorTo; G.drag = null; return false; }
      G.anchor = anchorTo; G.sel = union(G.base, next(anchorTo)); }
    else { G.base = new Set(); G.anchor = anchorTo; G.sel = next(anchorTo); }
    return true; }
  function renew(t){ var sig = t.getAttribute('data-sig'); index(t);
    if (sig === G.sig) { G.sel.forEach(function(k){ if (!G.cells[k]) G.sel.delete(k); }); paint(); }
    else { G.sig = sig; G.sel = new Set(); G.base = new Set(); G.active = null; G.anchor = null; paint(); send(); } }
  $(document).on('shiny:value', function(){ setTimeout(function(){ var t = document.querySelector('table.wb-gt'); if (t && t !== G.table) renew(t); }, 0); });
  $(document).on('mousedown', 'table.wb-gt td[data-c]', function(e){ if (e.button !== 0) return; e.preventDefault();
    var p = {r: +this.parentNode.getAttribute('data-r'), c: +this.getAttribute('data-c')};
    var go = pick(function(a){ return rect(a, p); }, e, p); G.active = p; G.drag = go ? 'cell' : null; paint(); focus(); if (!go) later(); });
  $(document).on('mouseover', 'table.wb-gt td[data-c]', function(){ if (G.drag !== 'cell') return;
    var p = {r: +this.parentNode.getAttribute('data-r'), c: +this.getAttribute('data-c')}; G.active = p; G.sel = union(G.base, rect(G.anchor, p)); paint(); });
  $(document).on('mousedown', 'table.wb-gt th.rh', function(e){ if (e.button !== 0) return; e.preventDefault(); var r = +this.parentNode.getAttribute('data-r');
    var go = pick(function(a){ return rows(a.r, r); }, e, {r: r, c: 1}); G.active = {r: r, c: 1}; G.drag = go ? 'row' : null; paint(); focus(); if (!go) later(); });
  $(document).on('mouseover', 'table.wb-gt th.rh', function(){ if (G.drag !== 'row') return; var r = +this.parentNode.getAttribute('data-r');
    G.active = {r: r, c: 1}; G.sel = union(G.base, rows(G.anchor.r, r)); paint(); });
  $(document).on('mousedown', 'table.wb-gt th.ch', function(e){ if (e.button !== 0) return; e.preventDefault(); var c = +this.getAttribute('data-c');
    var go = pick(function(a){ return cols(a.c, c); }, e, {r: 1, c: c}); G.active = {r: 1, c: c}; G.drag = go ? 'col' : null; paint(); focus(); if (!go) later(); });
  $(document).on('mouseover', 'table.wb-gt th.ch', function(){ if (G.drag !== 'col') return; var c = +this.getAttribute('data-c');
    G.active = {r: 1, c: c}; G.sel = union(G.base, cols(G.anchor.c, c)); paint(); });
  $(document).on('mousedown', 'table.wb-gt th.cn', function(e){ e.preventDefault(); G.base = new Set(); G.anchor = {r: 1, c: 1}; G.active = {r: 1, c: 1};
    G.sel = rect({r: 1, c: 1}, {r: G.nr, c: G.nc}); paint(); focus(); later(); });
  $(document).on('mouseup', function(){ if (G.drag) { G.drag = null; later(); } });
  $(document).on('keydown', '.wb-gwrap', function(e){
    if (e.key === 'Escape') { G.sel = new Set(); G.base = new Set(); G.active = null; paint(); later(); return; }
    if ((e.metaKey || e.ctrlKey) && (e.key === 'a' || e.key === 'A')) { e.preventDefault(); G.base = new Set(); G.sel = rect({r: 1, c: 1}, {r: G.nr, c: G.nc}); paint(); later(); return; }
    var d = {ArrowUp: [-1, 0], ArrowDown: [1, 0], ArrowLeft: [0, -1], ArrowRight: [0, 1]}[e.key]; if (!d || !G.nr) return; e.preventDefault();
    var a = G.active || {r: 1, c: 1}, n = {r: Math.min(G.nr, Math.max(1, a.r + d[0])), c: Math.min(G.nc, Math.max(1, a.c + d[1]))};
    if (e.shiftKey && G.anchor) G.sel = union(G.base, rect(G.anchor, n)); else { G.base = new Set(); G.anchor = n; G.sel = rect(n, n); }
    G.active = n; paint(); var td = G.cells[K(n.r, n.c)]; if (td) td.scrollIntoView({block: 'nearest', inline: 'nearest'}); later(); });
  $(function(){ if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler('wb-grid-clear', function(m){ G.sel = new Set(); G.base = new Set(); G.active = null; paint(); send(); });   // Shiny wants one argument
    Shiny.addCustomMessageHandler('wb-open', wbOpen);
    Shiny.addCustomMessageHandler('wb-undo', wbUndoBar); } });
  // After an action is written: what was done, with Undo (or Ctrl/Cmd+Z while the bar shows); plot standards A11
  var UNDO_T = null;
  function wbUndoBar(m){ var b = document.getElementById('wb-undo'); if (!b) { b = document.createElement('div'); b.id = 'wb-undo'; b.className = 'wb-undo'; b.setAttribute('role', 'status');
      b.innerHTML = '<span></span><button type="button" title="Ctrl/Cmd+Z">Undo</button><button type="button" class="x" aria-label="Close">\u00d7</button>'; document.body.appendChild(b);
      b.querySelector('button').addEventListener('click', wbUndo); b.querySelector('.x').addEventListener('click', function(){ b.hidden = true; }); }
    b.querySelector('span').textContent = m.msg; b.hidden = false; clearTimeout(UNDO_T); UNDO_T = setTimeout(function(){ b.hidden = true; }, 15000); }
  function wbUndo(){ var b = document.getElementById('wb-undo'); if (!b || b.hidden) return false; b.hidden = true; Shiny.setInputValue('wb_undo', Date.now(), {priority: 'event'}); return true; }
  $(document).on('keydown', function(e){ if ((e.metaKey || e.ctrlKey) && !e.shiftKey && (e.key === 'z' || e.key === 'Z') && !$(e.target).is('input,textarea,select,[contenteditable]') && wbUndo()) e.preventDefault(); });
})();


// the side panels: fold to a 28 px strip (the buttons, Alt+[ the tree, Alt+] the inspector), resize by the inner edge (180 to
// 480 px) and float the inspector over the page. Wide pages (all but Data and Import) show the inspector as its strip; opening
// it there is kept apart. Kept in this browser (localStorage, the grid's key prefix); without it the panels start open.
(function(){ var APP = (document.querySelector('.wb-brand .app') || {}).textContent || 'app', KEY = 'ovid.grid.' + APP + '.panels';
  var st = { nav: true, aside: true, asideWide: false, float: false, x: null, y: null, wnav: null, waside: null };
  try { var v = JSON.parse(localStorage.getItem(KEY) || 'null'); if (v) for (var k in st) if (k in v) st[k] = v[k]; } catch (x) {}
  function save(){ try { localStorage.setItem(KEY, JSON.stringify(st)); } catch (x) {} }
  function refit(){ setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 60); }
  function body(){ return document.querySelector('.wb-body'); }
  function apply(){ var b = body(); if (!b) return; var wide = b.classList.contains('wide'), aside = wide ? st.asideWide : st.aside;
    b.classList.toggle('nav-off', !st.nav); b.classList.toggle('aside-off', !aside); b.classList.toggle('aside-float', aside && st.float);
    var n = b.querySelector('.wb-nav'), a = b.querySelector('.wb-aside');
    if (n) n.style.width = st.wnav ? st.wnav + 'px' : ''; if (a) { a.style.width = st.waside ? st.waside + 'px' : '';
      a.style.left = st.float && st.x != null ? st.x + 'px' : ''; a.style.top = st.float && st.y != null ? st.y + 'px' : ''; a.style.right = st.float && st.x != null ? 'auto' : ''; }
    b.querySelectorAll('[data-wb-fold="nav"]').forEach(function(e){ e.setAttribute('aria-expanded', st.nav); });
    b.querySelectorAll('[data-wb-unfold="nav"]').forEach(function(e){ e.setAttribute('aria-expanded', st.nav); });
    b.querySelectorAll('[data-wb-fold="aside"], [data-wb-unfold="aside"]').forEach(function(e){ e.setAttribute('aria-expanded', aside); });
    b.querySelectorAll('[data-wb-float]').forEach(function(e){ e.setAttribute('aria-pressed', st.float); e.setAttribute('aria-label', st.float ? 'Dock the inspector' : 'Float the inspector over the page'); }); }
  function set(p, open){ if (p === 'nav') st.nav = open; else if (body() && body().classList.contains('wide')) st.asideWide = open; else st.aside = open; save(); apply(); refit(); }
  function isOpen(p){ var b = body(); return p === 'nav' ? st.nav : (b && b.classList.contains('wide') ? st.asideWide : st.aside); }
  window.wbPanels = { apply: apply, set: set, state: function(){ return JSON.parse(JSON.stringify(st)); } };
  $(document).on('click', '[data-wb-fold]', function(){ set(this.getAttribute('data-wb-fold'), false); });
  $(document).on('click', '[data-wb-unfold]', function(){ set(this.getAttribute('data-wb-unfold'), true); });
  $(document).on('click', '[data-wb-float]', function(){ st.float = !st.float; save(); apply(); refit(); });
  document.addEventListener('keydown', function(e){ if (!e.altKey || e.ctrlKey || e.metaKey) return;
    var p = e.code === 'BracketLeft' ? 'nav' : e.code === 'BracketRight' ? 'aside' : null; if (p) { e.preventDefault(); set(p, !isOpen(p)); } });
  $(document).on('pointerdown', '[data-wb-grip]', function(e){ var g = this, p = g.getAttribute('data-wb-grip'), el = body().querySelector(p === 'nav' ? '.wb-nav' : '.wb-aside');
    if (!el) return; e.preventDefault(); var x0 = e.clientX, w0 = el.offsetWidth; g.classList.add('on');
    function mv(ev){ var w = Math.max(180, Math.min(480, w0 + (p === 'nav' ? ev.clientX - x0 : x0 - ev.clientX))); el.style.width = w + 'px'; st['w' + p] = w; }
    function up(){ g.classList.remove('on'); document.removeEventListener('pointermove', mv); document.removeEventListener('pointerup', up); save(); refit(); }
    document.addEventListener('pointermove', mv); document.addEventListener('pointerup', up); });
  $(document).on('pointerdown', '.wb-aside .wb-ph', function(e){ var b = body(); if (!st.float || e.target.closest('button') || !b) return; e.preventDefault();
    var a = this.closest('.wb-aside'), r = a.getBoundingClientRect(), bb = b.getBoundingClientRect(), dx = e.clientX - r.left, dy = e.clientY - r.top;
    function mv(ev){ st.x = Math.max(0, Math.min(bb.width - r.width, ev.clientX - bb.left - dx)); st.y = Math.max(0, Math.min(bb.height - 40, ev.clientY - bb.top - dy)); apply(); }
    function up(){ document.removeEventListener('pointermove', mv); document.removeEventListener('pointerup', up); save(); }
    document.addEventListener('pointermove', mv); document.addEventListener('pointerup', up); });
  $(apply); })();
