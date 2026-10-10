
(function(){
  var S = {};
  function colL(i){ var s = ''; i++; while (i > 0) { var m = (i - 1) % 26; s = String.fromCharCode(65 + m) + s; i = Math.floor((i - 1) / 26); } return s; }
  function isNum(v){ return v !== '' && v != null && !isNaN(+v) && isFinite(+v); }
  function r2(v, d){ var p = Math.pow(10, d == null ? 3 : d); return Math.round(v * p) / p; }
  function esc(s){ return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/"/g, '&quot;'); }
  // a value that is not one of its column's set choices (case aside); blanks are left to the table checks
  function badCh(c, raw){ if (!c || !c.ch || raw === '' || raw == null) return false; var l = String(raw).trim().toLowerCase(); return !c.ch.some(function(x){ return String(x).toLowerCase() === l; }); }
  function nBad(T){ var n = 0; T.rows.forEach(function(r){ T.cols.forEach(function(c, ci){ if (c.ed && badCh(c, r[ci])) n++; }); }); return n; }
  function cellVal(T, r, c){ return T.rows[r][c]; }
  // ---- the sheet ----
  function el(T, q){ return T.box.querySelector(q); }
  function view(T){ var f = (el(T, '.ds-find').value || '').toLowerCase(), v = T.rows.map(function(_, i){ return i; });
    if (f) v = v.filter(function(i){ return T.rows[i].some(function(_, c){ return String(cellVal(T, i, c)).toLowerCase().indexOf(f) >= 0; }); });
    var fl = T.filt || {}; Object.keys(fl).forEach(function(c){ var q = (fl[c] || '').trim(); if (q) v = v.filter(function(i){ return ftest(q, cellVal(T, i, +c)); }); });
    var so = T.sort ? [].concat(T.sort) : [];
    if (so.length) v.sort(function(a, b){ for (var k = 0; k < so.length; k++) { var c = so[k].c, x = cellVal(T, a, c), y = cellVal(T, b, c),
      r = (isNum(x) && isNum(y) ? x - y : String(x).localeCompare(String(y))) * so[k].d; if (r) return r; } return a - b; });
    return v; }
  // a column filter: >5, <=0, 1..3, =2, !=0, or text the cell holds
  function ftest(q, v){ var m, x = isNum(v) ? +v : NaN;
    if ((m = /^(-?[\d.]+)\s*\.\.\s*(-?[\d.]+)$/.exec(q))) return x >= +m[1] && x <= +m[2];
    if ((m = /^(>=|<=|!=|>|<|=)\s*(-?[\d.]+)$/.exec(q))) { var y = +m[2]; return m[1] === '>' ? x > y : m[1] === '<' ? x < y : m[1] === '>=' ? x >= y : m[1] === '<=' ? x <= y : m[1] === '=' ? x === y : x !== y; }
    return String(v).toLowerCase().indexOf(q.toLowerCase()) >= 0; }
  function fmtv(T, ci, val){ var d = T.fmt && T.fmt[ci]; if (d === undefined || d === null || d === '' || !isNum(val)) return val; return (+val).toFixed(+d); }
  function sortMark(T, ci){ var so = T.sort ? [].concat(T.sort) : [], k = -1; so.forEach(function(x, i){ if (x.c === ci) k = i; }); if (k < 0) return '';
    return (so[k].d > 0 ? ' \u25b2' : ' \u25bc') + (so.length > 1 ? '<sup>' + (k + 1) + '</sup>' : ''); }
  function inSel(T, vr, c){ var s = T.sel; return s.r != null && vr >= Math.min(s.r, s.r2) && vr <= Math.max(s.r, s.r2) && c >= Math.min(s.c, s.c2) && c <= Math.max(s.c, s.c2); }
  function render(T){ var g = el(T, '.ds-grid');
    if (T.none) { g.innerHTML = '<div class="ds-none">This table is created the first time the pipeline runs.</div>'; bars(T); return; }
    var v = view(T); T.view = v; var s = T.sel, h = ['<table><tr><th class="rn"></th>'];
    T.cols.forEach(function(c, ci){ if (T.hidden && T.hidden[ci]) return; var hc = s.r != null && ci >= Math.min(s.c, s.c2) && ci <= Math.max(s.c, s.c2);
      var wd = T.w && T.w[c.l] ? ' style="width:' + T.w[c.l] + 'px;min-width:' + T.w[c.l] + 'px;max-width:' + T.w[c.l] + 'px"' : '';
      h.push('<th class="cl' + (hc ? ' hc' : '') + '" data-c="' + ci + '"' + wd + ' title="' + esc(c.l) + (c.ed ? '' : ' (read only)') + ' \u00b7 click to sort, shift-click to add a sort"><span class="lt">' + colL(ci) + '</span>' + esc(c.l) + sortMark(T, ci) + '<span class="ds-rsz" title="Drag to resize; double-click to fit"></span></th>'); });
    h.push('</tr><tr class="ds-frow"><th class="rn"></th>');
    T.cols.forEach(function(c, ci){ if (T.hidden && T.hidden[ci]) return; h.push('<th><input class="ds-filt" data-c="' + ci + '" value="' + esc((T.filt || {})[ci] || '') + '" placeholder="filter" title=">5, <=0, 1..3, =2, or text"></th>'); });
    h.push('</tr>');
    v.forEach(function(ri, vr){ var rs = s.r != null && vr >= Math.min(s.r, s.r2) && vr <= Math.max(s.r, s.r2);
      h.push('<tr><td class="rn' + (rs ? ' hr' : '') + '">' + (ri + 1) + '</td>');
      T.cols.forEach(function(c, ci){ if (T.hidden && T.hidden[ci]) return; var raw = T.rows[ri][ci], val = cellVal(T, ri, ci), base = T.base[ri], chg = !base || raw !== base[ci];
        var cls = ['c', c.ed ? '' : 'ro', c.t === 'num' ? 'nr' : '', chg ? 'pd' : '', (vr === s.r && ci === s.c) ? 'cur' : '', inSel(T, vr, ci) ? 'in' : '', c.ch && c.ed ? 'chc' : '', c.ed && badCh(c, raw) ? 'bad' : ''].join(' ');
        var tip = c.ed && badCh(c, raw) ? '\u201c' + raw + '\u201d is not one of: ' + c.ch.join(', ') + '. Double-click and pick one.' : val;
        var shown = fmtv(T, ci, val); h.push('<td class="' + cls + '" data-r="' + vr + '" data-c="' + ci + '" title="' + esc(tip) + '">' + (val === '' ? '<span class="m">\u2014</span>' : esc(shown)) + '</td>'); });
      h.push('</tr>'); });
    cols(T); h.push('</table>'); var st = g.scrollTop, sl = g.scrollLeft, ff = T.ffoc; g.innerHTML = h.join(''); g.scrollTop = st; g.scrollLeft = sl;
    if (ff != null) { var fi = g.querySelector('.ds-filt[data-c="' + ff + '"]'); if (fi) { fi.focus(); fi.setSelectionRange(fi.value.length, fi.value.length); } }
    ['compact', 'normal', 'tall'].forEach(function(x){ g.classList.toggle('ds-' + x, (T.rh || 'normal') === x); }); g.classList.toggle('ds-wrap', !!T.wrap);
    var hh = g.querySelector('th.cl'); if (hh) g.querySelectorAll('tr.ds-frow th').forEach(function(x){ x.style.top = hh.offsetHeight + 'px'; }); bars(T); }
  function cols(T){ var p = T.box.querySelector('.ds-cols-pop'); if (!p || p._n === T.cols.length + ':' + T.id) return; p._n = T.cols.length + ':' + T.id; T.hidden = T.hidden || {};
    T.fmt = T.fmt || {};
    p.innerHTML = T.cols.map(function(c, ci){ return '<label><input type="checkbox" data-ci="' + ci + '"' + (T.hidden[ci] ? '' : ' checked') + '> ' + colL(ci) + ' ' + esc(c.l) +
      (c.t === 'num' ? ' <select data-fi="' + ci + '" title="Decimals shown (the table keeps its values)"><option value="">as typed</option>' + [0, 1, 2, 3, 4].map(function(d){ return '<option value="' + d + '">' + d + ' dp</option>'; }).join('') + '</select>' : '') + '</label>'; }).join('');
    p.onchange = function(e){ if (e.target.hasAttribute('data-fi')) { T.fmt[+e.target.getAttribute('data-fi')] = e.target.value; return render(T); }
      var ci = +e.target.getAttribute('data-ci'); T.hidden[ci] = !e.target.checked; render(T); }; }
  function pending(T){ var n = 0, kept = 0; T.rows.forEach(function(r, i){ var b = T.base[i]; if (!b) n++; else { kept++; r.forEach(function(x, c){ if (x !== b[c]) n++; }); } }); return n + Math.max(0, (T.savedN || 0) - kept); }
  function bars(T){ var s = T.sel, nm = el(T, '.ds-nm'), inp = el(T, '.ds-in'), st = el(T, '.ds-stats');
    if (!T.none && T.view && s.r != null && T.view[s.r] != null) { var ri = T.view[s.r], c = T.cols[s.c], raw = T.rows[ri][s.c];
      nm.textContent = colL(s.c) + (ri + 1) + ' \u00b7 ' + (T.label || 'table') + ' \u00b7 ' + c.l + (c.ed ? '' : ' (read only)');
      if (document.activeElement !== inp) inp.value = raw; inp.readOnly = !c.ed;
      var nums = []; for (var a = Math.min(s.r, s.r2); a <= Math.max(s.r, s.r2); a++) for (var b = Math.min(s.c, s.c2); b <= Math.max(s.c, s.c2); b++) { if (T.view[a] == null) continue; var x = cellVal(T, T.view[a], b); if (isNum(x)) nums.push(+x); }
      var n = nums.length, sum = 0; nums.forEach(function(x){ sum += x; });
      st.textContent = (T.msg ? T.msg : n > 1 ? 'count ' + n + ' \u00b7 sum ' + r2(sum, 4) + ' \u00b7 mean ' + r2(sum / n, 4) + ' \u00b7 min ' + Math.min.apply(null, nums) + ' \u00b7 max ' + Math.max.apply(null, nums) : n === 1 ? 'value ' + nums[0] : (T.msg || (T.view.length + ' of ' + T.rows.length + ' rows'))); }
    else { nm.textContent = '\u2014'; inp.value = ''; st.textContent = T.msg || (T.none ? '' : T.rows.length + ' rows'); }
    T.msg = '';
    var p = pending(T), ap = T.box.querySelector('[data-ds="apply"]');
    var nb = nBad(T); ap.textContent = p > 0 ? 'Apply ' + p + ' change' + (p > 1 ? 's' : '') : 'Apply'; ap.disabled = p <= 0 || T.ro || nb > 0;
    T.box.querySelector('[data-ds="undo"]').disabled = !T.hist.length; T.box.querySelector('[data-ds="redo"]').disabled = !T.redo.length;
    var why = function(sel, off, t){ var b = T.box.querySelector(sel); if (!b) return; if (!b.hasAttribute('data-t0')) b.setAttribute('data-t0', b.title || ''); b.title = off ? t : b.getAttribute('data-t0'); };
    why('[data-ds="apply"]', nb > 0, nb + ' value' + (nb > 1 ? 's are' : ' is') + ' not one of the column\u2019s choices (outlined in red): double-click each and pick one');
    if (nb > 0 && st && st.textContent.indexOf('not one of the choices') < 0) st.textContent = nb + ' value' + (nb > 1 ? 's' : '') + ' not one of the choices (red): double-click and pick one \u00b7 ' + st.textContent;
    why('[data-ds="undo"]', !T.hist.length, 'Nothing to undo since the table was read or applied'); why('[data-ds="redo"]', !T.redo.length, 'Nothing to redo');
    if (!(nb > 0)) why('[data-ds="apply"]', ap.disabled, T.ro ? 'This copy is read-only (viewer)' : 'No changes to apply yet');
    T.box.querySelector('[data-ds="addrow"]').disabled = T.ro || T.none; T.box.querySelector('[data-ds="delrows"]').disabled = T.ro || T.none || s.r == null; why('[data-ds="delrows"]', s.r == null && !T.ro, 'Select cells in the rows to delete first'); }
  function fillDown(T){ var s = T.sel; if (s.r == null || T.ro) return; var r0 = Math.min(s.r, s.r2), r1 = Math.max(s.r, s.r2); if (r1 === r0) { T.msg = 'select the cells to fill, the first one holding the value'; return bars(T); }
    var before = snap(T), n = 0; for (var b = Math.min(s.c, s.c2); b <= Math.max(s.c, s.c2); b++) { if (!T.cols[b].ed) continue; var top = T.rows[T.view[r0]][b];
      for (var a = r0 + 1; a <= r1; a++) { var ri = T.view[a]; if (T.rows[ri][b] !== top) { T.rows[ri][b] = top; n++; } } }
    if (n) push(T, {k: 'rows', before: before}); T.msg = n ? 'filled ' + n + ' cell' + (n > 1 ? 's' : '') + ' down (Undo takes it back)' : 'nothing to fill'; render(T); }
  function replaceAll(T){ var f = el(T, '.ds-find').value, r = el(T, '.ds-repl').value; if (!f || T.ro) { T.msg = f ? 'read only' : 'type what to find first'; return bars(T); }
    var before = snap(T), n = 0, lo = f.toLowerCase(), rx = new RegExp(f.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'), 'gi');
    T.view.forEach(function(ri){ T.cols.forEach(function(c, ci){ if (!c.ed) return; var v = String(T.rows[ri][ci]); if (v.toLowerCase().indexOf(lo) < 0) return;
      var nv = v.replace(rx, r); if (nv !== v) { T.rows[ri][ci] = nv; n++; } }); });
    if (n) push(T, {k: 'rows', before: before}); T.msg = n ? 'replaced in ' + n + ' cell' + (n > 1 ? 's' : '') + ' (Undo takes it back)' : 'no editable cell holds it'; render(T); }
  function exportCsv(T){ var q = function(x){ x = String(x == null ? '' : x); return /[",\n]/.test(x) ? '"' + x.replace(/"/g, '""') + '"' : x; };
    var vis = T.cols.map(function(_, ci){ return ci; }).filter(function(ci){ return !(T.hidden && T.hidden[ci]); });
    var lines = [vis.map(function(ci){ return q(T.cols[ci].l); }).join(',')].concat(T.view.map(function(ri){ return vis.map(function(ci){ return q(cellVal(T, ri, ci)); }).join(','); }));
    var a = document.createElement('a'); a.href = URL.createObjectURL(new Blob([lines.join('\n') + '\n'], {type: 'text/csv'})); a.download = (T.label || 'table').replace(/\.csv$/, '') + '_view.csv'; document.body.appendChild(a); a.click(); a.remove();
    T.msg = 'saved ' + T.view.length + ' rows as it shows (filters, sort and columns as they are)'; bars(T); }
  function wkey(T){ return 'ovid.sheet.' + (T.label || T.id); }
  function wsave(T){ try { localStorage.setItem(wkey(T), JSON.stringify({w: T.w || {}, rh: T.rh || 'normal', wrap: !!T.wrap})); } catch (x) {} }
  function wload(T){ try { var x = JSON.parse(localStorage.getItem(wkey(T)) || '{}'); T.w = x.w || {}; T.rh = x.rh || 'normal'; T.wrap = !!x.wrap; } catch (x) { T.w = {}; T.rh = 'normal'; } }
  function snap(T){ return {rows: T.rows.map(function(r){ return r.slice(); }), base: T.base.map(function(r){ return r ? r.slice() : null; })}; }
  function push(T, h){ T.hist.push(h); T.redo = []; sync(T); }
  // changes not applied yet go to the server as the table's unsaved edits (debounced), so the other view opens with them
  function sync(T){ T.unsynced = true; clearTimeout(T.st); T.st = setTimeout(function(){ flush(T); }, 400); }
  function flush(T){ if (!T.unsynced || !window.Shiny) return false; T.unsynced = false; clearTimeout(T.st);
    Shiny.setInputValue(T.id + '_pending', {rows: T.rows.map(function(r){ return r.slice(); }), dirty: pending(T) > 0, n: Date.now()}, {priority: 'event'}); return true; }
  window.ovidSheetFlush = function(){ var any = false; Object.keys(S).forEach(function(k){ if (flush(S[k])) any = true; }); return any; };
  function setCell(T, ri, ci, val){ var c = T.cols[ci]; if (!c || !c.ed) { T.msg = 'column ' + (c ? c.l : '') + ' is read only'; return false; }
    var old = T.rows[ri][ci]; if (old === val) return false; T.rows[ri][ci] = val; push(T, {k: 'cell', ri: ri, ci: ci, old: old, nw: val}); return true; }
  function undo(T){ var h = T.hist.pop(); if (!h) return; sync(T); if (h.k === 'cell') T.rows[h.ri][h.ci] = h.old; else { var cur = snap(T); T.rows = h.before.rows; T.base = h.before.base; h.after = cur; } T.redo.push(h); render(T); }
  function redo(T){ var h = T.redo.pop(); if (!h) return; sync(T); if (h.k === 'cell') T.rows[h.ri][h.ci] = h.nw; else { T.rows = h.after.rows; T.base = h.after.base; } T.hist.push(h); render(T); }
  function apply(T){ if (T.ro || pending(T) <= 0 || nBad(T) > 0) return;
    var out = T.rows.map(function(r){ return r.slice(); });
    Shiny.setInputValue(T.id + '_apply', {rows: out, n: Date.now()}, {priority: 'event'}); T.msg = 'saving\u2026'; bars(T); }
  function edit(T, vr, c, first){ var col = T.cols[c], ri = T.view[vr]; if (ri == null) return; if (!col.ed) { T.msg = 'column ' + col.l + ' is read only'; bars(T); return; }
    var td = T.box.querySelector('td[data-r="' + vr + '"][data-c="' + c + '"]'); if (!td) return; var raw = T.rows[ri][c];
    if (col.ch) { var opts = (raw === '' || !badCh(col, raw) ? [''] : [raw]).concat(col.ch), lo = String(first != null ? first : raw).toLowerCase(), pick = '';
      col.ch.forEach(function(x){ if (!pick && first != null && first !== '' && String(x).toLowerCase().indexOf(lo) === 0) pick = x; }); if (!pick) pick = raw;
      td.innerHTML = '<select class="ce">' + opts.map(function(x){ return '<option value="' + esc(x) + '"' + (String(x) === String(pick) ? ' selected' : '') + '>' + (x === '' ? '(blank)' : esc(x)) + (badCh(col, x) ? ' (not a choice)' : '') + '</option>'; }).join('') + '</select>';
      var e = td.firstChild; e.focus(); try { if (first == null && e.showPicker) e.showPicker(); } catch (x) {} }
    else { td.innerHTML = '<input class="ce" value="' + esc(first != null ? first : raw) + '">'; var e = td.firstChild; e.focus(); if (first == null) e.select(); }
    var done = false; function fin(ok){ if (done) return; done = true; if (ok) setCell(T, ri, c, e.value.trim()); render(T); el(T, '.ds-grid').focus(); }
    e.addEventListener('keydown', function(ev){ ev.stopPropagation(); if (ev.key === 'Enter') { fin(true); if (T.sel.r < T.view.length - 1) { T.sel.r++; T.sel.r2 = T.sel.r; T.sel.c2 = T.sel.c; render(T); } } if (ev.key === 'Escape') fin(false); if (ev.key === 'Tab') { ev.preventDefault(); fin(true); move(T, 0, 1, false); } });
    e.addEventListener('blur', function(){ fin(true); }); if (col.ch) e.addEventListener('change', function(){ fin(true); }); }
  function move(T, dr, dc, ext){ var s = T.sel, n = T.view.length, m = T.cols.length; if (s.r == null) { s.r = s.r2 = 0; s.c = s.c2 = 0; render(T); return; }
    var r = Math.max(0, Math.min(n - 1, (ext ? s.r2 : s.r) + dr)), c = Math.max(0, Math.min(m - 1, (ext ? s.c2 : s.c) + dc));
    if (ext) { s.r2 = r; s.c2 = c; } else { s.r = s.r2 = r; s.c = s.c2 = c; } render(T);
    var td = T.box.querySelector('td[data-r="' + (ext ? s.r2 : s.r) + '"][data-c="' + (ext ? s.c2 : s.c) + '"]'); if (td) td.scrollIntoView({block: 'nearest', inline: 'nearest'}); }
  function wire(T){ var g = el(T, '.ds-grid'), inp = el(T, '.ds-in');
    g.addEventListener('mousedown', function(e){ var td = e.target.closest('td.c'); if (!td || e.target.closest('input')) return; var vr = +td.dataset.r, c = +td.dataset.c, s = T.sel;
      if (e.shiftKey && s.r != null) { s.r2 = vr; s.c2 = c; } else { s.r = s.r2 = vr; s.c = s.c2 = c; } T.drag = true; render(T); g.focus(); e.preventDefault(); });
    g.addEventListener('mouseover', function(e){ if (!T.drag) return; var td = e.target.closest('td.c'); if (!td) return; T.sel.r2 = +td.dataset.r; T.sel.c2 = +td.dataset.c; render(T); });
    document.addEventListener('mouseup', function(){ T.drag = false; });
    g.addEventListener('dblclick', function(e){ var td = e.target.closest('td.c'); if (td) edit(T, +td.dataset.r, +td.dataset.c); });
    g.addEventListener('click', function(e){ if (e.target.closest('.ds-rsz')) return; var th = e.target.closest('th.cl'); if (!th) return; var c = +th.dataset.c, so = T.sort ? [].concat(T.sort) : [], k = -1;
      so.forEach(function(x, i){ if (x.c === c) k = i; });
      if (e.shiftKey) { if (k < 0) so.push({c: c, d: 1}); else if (so[k].d > 0) so[k].d = -1; else so.splice(k, 1); }
      else so = k >= 0 && so.length === 1 ? (so[0].d > 0 ? [{c: c, d: -1}] : []) : [{c: c, d: 1}];
      T.sort = so.length ? so : null; T.sel.r = T.sel.r2 = null; render(T); });
    g.addEventListener('input', function(e){ var f = e.target.closest('.ds-filt'); if (!f) return; T.filt = T.filt || {}; T.filt[f.getAttribute('data-c')] = f.value; T.ffoc = f.getAttribute('data-c'); T.sel.r = T.sel.r2 = null; render(T); });
    g.addEventListener('focusout', function(e){ if (e.target.closest('.ds-filt')) T.ffoc = null; });
    g.addEventListener('mousedown', function(e){ var hz = e.target.closest('.ds-rsz'); if (!hz) return; e.preventDefault(); e.stopPropagation(); var th = hz.closest('th'), x0 = e.clientX, w0 = th.getBoundingClientRect().width, nm = T.cols[+th.dataset.c].l;
      function mv(ev){ var w = Math.max(40, w0 + ev.clientX - x0); th.style.width = th.style.minWidth = th.style.maxWidth = w + 'px'; }
      function up(){ document.removeEventListener('mousemove', mv); document.removeEventListener('mouseup', up); T.w = T.w || {}; T.w[nm] = Math.round(th.getBoundingClientRect().width); wsave(T); render(T); }
      document.addEventListener('mousemove', mv); document.addEventListener('mouseup', up); }, true);
    g.addEventListener('dblclick', function(e){ var hz = e.target.closest('.ds-rsz'); if (!hz) return; e.stopPropagation(); var th = hz.closest('th'), ci = th.dataset.c, nm = T.cols[+ci].l, mx = 40;
      T.w = T.w || {}; delete T.w[nm]; render(T); var g2 = el(T, '.ds-grid'), cx = document.createElement('canvas').getContext('2d'); g2.querySelectorAll('td[data-c="' + ci + '"], th.cl[data-c="' + ci + '"]').forEach(function(x){ mx = Math.max(mx, (cx.font = getComputedStyle(x).font, cx.measureText(x.textContent).width) + 24); });
      T.w[nm] = Math.round(Math.min(600, mx)); wsave(T); render(T); }, true);
    g.addEventListener('keydown', function(e){ var s = T.sel, mod = e.metaKey || e.ctrlKey, k = e.key;
      if (mod && k.toLowerCase() === 'z') { e.preventDefault(); return e.shiftKey ? redo(T) : undo(T); }
      if (mod && k.toLowerCase() === 'y') { e.preventDefault(); return redo(T); }
      if (mod && k.toLowerCase() === 's') { e.preventDefault(); return apply(T); }
      if (s.r == null) { if (/^Arrow/.test(k)) { e.preventDefault(); move(T, 0, 0, false); } return; }
      if (k === 'ArrowDown') { e.preventDefault(); return move(T, 1, 0, e.shiftKey); } if (k === 'ArrowUp') { e.preventDefault(); return move(T, -1, 0, e.shiftKey); }
      if (k === 'ArrowLeft') { e.preventDefault(); return move(T, 0, -1, e.shiftKey); } if (k === 'ArrowRight' || k === 'Tab') { e.preventDefault(); return move(T, 0, e.shiftKey && k === 'Tab' ? -1 : 1, e.shiftKey && k !== 'Tab'); }
      if (mod && k.toLowerCase() === 'c') { var lines = []; for (var a = Math.min(s.r, s.r2); a <= Math.max(s.r, s.r2); a++) { var row = []; for (var b = Math.min(s.c, s.c2); b <= Math.max(s.c, s.c2); b++) row.push(cellVal(T, T.view[a], b)); lines.push(row.join('\t')); }
        var txt = lines.join('\n'); if (navigator.clipboard && navigator.clipboard.writeText) navigator.clipboard.writeText(txt).catch(function(){}); T.copied = txt; T.msg = 'copied ' + lines.length + ' row' + (lines.length > 1 ? 's' : ''); bars(T); return; }
      if (mod && k.toLowerCase() === 'd') { e.preventDefault(); return fillDown(T); }
      if (mod && k.toLowerCase() === 'a') { e.preventDefault(); s.r = 0; s.c = 0; s.r2 = T.view.length - 1; s.c2 = T.cols.length - 1; return render(T); }
      if (k === 'Enter' || k === 'F2') { e.preventDefault(); return edit(T, s.r, s.c); }
      if (k === 'Escape') { s.r2 = s.r; s.c2 = s.c; return render(T); }
      if (k === 'Delete' || k === 'Backspace') { e.preventDefault(); var before = snap(T), any = false;
        for (var a2 = Math.min(s.r, s.r2); a2 <= Math.max(s.r, s.r2); a2++) for (var b2 = Math.min(s.c, s.c2); b2 <= Math.max(s.c, s.c2); b2++) if (T.cols[b2].ed && T.rows[T.view[a2]][b2] !== '') { T.rows[T.view[a2]][b2] = ''; any = true; }
        if (any) push(T, {k: 'rows', before: before}); return render(T); }
      if (k.length === 1 && !mod && !e.altKey) { e.preventDefault(); if (!T.cols[s.c].ed) { T.msg = 'column ' + T.cols[s.c].l + ' is read only'; return bars(T); } edit(T, s.r, s.c, k); } });
    g.addEventListener('paste', function(e){ var s = T.sel; if (s.r == null || T.ro) return; e.preventDefault();
      var txt = (e.clipboardData || window.clipboardData).getData('text'), before = snap(T), n = 0;
      txt.replace(/\r/g, '').split('\n').filter(function(x, i, a){ return x !== '' || i < a.length - 1; }).forEach(function(line, a){ line.split('\t').forEach(function(v, b){
        var vr = Math.min(s.r, s.r2) + a, c = Math.min(s.c, s.c2) + b; if (T.view[vr] == null) { if (T.sort || el(T, '.ds-find').value) return; T.rows.push(T.cols.map(function(){ return ''; })); T.base.push(null); T.view.push(T.rows.length - 1); }
        if (T.cols[c] && T.cols[c].ed) { T.rows[T.view[vr]][c] = v.trim(); n++; } }); });
      if (n) push(T, {k: 'rows', before: before}); T.msg = 'pasted ' + n + ' cell' + (n === 1 ? '' : 's'); render(T); });
    inp.addEventListener('keydown', function(e){ var s = T.sel; if (s.r == null) return;
      if (e.key === 'Enter') { e.preventDefault(); setCell(T, T.view[s.r], s.c, inp.value.trim()); if (s.r < T.view.length - 1) s.r++; s.r2 = s.r; s.c2 = s.c; inp.blur(); render(T); g.focus(); }
      if (e.key === 'Escape') { inp.blur(); bars(T); g.focus(); } });
    el(T, '.ds-find').addEventListener('input', function(){ T.sel.r = T.sel.r2 = null; render(T); });
    T.box.querySelector('.ds-replb').addEventListener('click', function(){ replaceAll(T); });
    el(T, '.ds-goto').addEventListener('change', function(){ var d = this.value; if (!d) return; var dc = T.cols.map(function(c, i){ return c.t === 'date' ? i : -1; }).filter(function(i){ return i >= 0; });
      if (!dc.length) { T.msg = 'this table has no date column'; return bars(T); }
      var hit = -1; T.view.forEach(function(ri, vr){ if (hit >= 0) return; if (dc.some(function(c){ var v = String(cellVal(T, ri, c)).slice(0, 10); return /^\d{4}-\d\d-\d\d/.test(v) && v >= d; })) hit = vr; });
      if (hit < 0) { T.msg = 'no row on or after ' + d; return bars(T); }
      T.sel.r = T.sel.r2 = hit; T.sel.c = T.sel.c2 = dc[0]; render(T); var td = T.box.querySelector('td[data-r="' + hit + '"][data-c="' + dc[0] + '"]'); if (td) td.scrollIntoView({block: 'center'}); g.focus(); });
    T.box.querySelector('.ds-sb').addEventListener('click', function(e){ var b = e.target.closest('[data-ds]'); if (!b || b.disabled) return; var a = b.getAttribute('data-ds'), s = T.sel;
      if (a === 'undo') undo(T); else if (a === 'redo') redo(T); else if (a === 'apply') apply(T);
      else if (a === 'fill') fillDown(T); else if (a === 'export') exportCsv(T);
      else if (a === 'rh') { var o = ['compact', 'normal', 'tall']; T.rh = o[(o.indexOf(T.rh || 'normal') + 1) % 3]; b.textContent = 'Rows: ' + T.rh; wsave(T); render(T); }
      else if (a === 'wrap') { T.wrap = !T.wrap; b.classList.toggle('on', T.wrap); wsave(T); render(T); }
      else if (a === 'addrow') { var before = snap(T); T.rows.push(T.cols.map(function(){ return ''; })); T.base.push(null); push(T, {k: 'rows', before: before}); T.sort = null; el(T, '.ds-find').value = ''; render(T); s.r = s.r2 = T.view.length - 1; s.c = s.c2 = 0; render(T); g.focus(); }
      else if (a === 'delrows' && s.r != null) { var before2 = snap(T), del = [];
        for (var a3 = Math.min(s.r, s.r2); a3 <= Math.max(s.r, s.r2); a3++) del.push(T.view[a3]);
        del.sort(function(x, y){ return y - x; }).forEach(function(ri){ T.rows.splice(ri, 1); T.base.splice(ri, 1); });
        T.removed = (T.removed || 0) + del.length; push(T, {k: 'rows', before: before2}); s.r = s.r2 = null; T.msg = del.length + ' row' + (del.length > 1 ? 's' : '') + ' deleted (Apply saves it)'; render(T); } }); }
  if (window.Shiny) Shiny.addCustomMessageHandler('dsheet', function(m){ var box = document.getElementById(m.id); if (!box) return;
    var T = S[m.id]; if (!T) { T = S[m.id] = {id: m.id, box: box, sel: {}, hist: [], redo: [], sort: null, label: box.getAttribute('data-label') || ''}; wire(T); }
    T.none = !!m.none; T.ro = !!m.ro; if (m.label) T.label = m.label; if (!T.wl) { wload(T); T.wl = true; var rb = box.querySelector('[data-ds="rh"]'); if (rb) rb.textContent = 'Rows: ' + T.rh; var wb = box.querySelector('[data-ds="wrap"]'); if (wb) wb.classList.toggle('on', !!T.wrap); } T.cols = m.cols || []; T.rows = (m.rows || []).map(function(r){ return (Array.isArray(r) ? r : [r]).map(String); });
    T.base = m.base ? m.base.map(function(r){ return (Array.isArray(r) ? r : [r]).map(String); }) : T.rows.map(function(r){ return r.slice(); }); T.savedN = T.base.length; T.hist = []; T.redo = []; T.unsynced = false;
    if (m.base && m.base.length < T.rows.length) for (var q = m.base.length; q < T.rows.length; q++) T.base[q] = null;
    if (T.sel.r != null && T.sel.r >= T.rows.length) T.sel = {}; render(T); });
})();

