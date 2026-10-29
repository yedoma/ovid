
(function(){
  function resize(){ setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 80); }
  function go(v){ var a = document.querySelector('#nav a[data-value="' + CSS.escape(v) + '"]'); if (a) a.click(); }
  window.ovidGo = go;
  var titles = null;
  function getTitles(){ if (!titles) { try { titles = JSON.parse(document.getElementById('d-titles').textContent); } catch(e) { titles = {}; } } return titles; }
  function mark(v){
    document.querySelectorAll('.d-rail-link').forEach(function(a){ var on = a.getAttribute('data-ovid-go') === v; a.classList.toggle('active', on);
      if (on) a.setAttribute('aria-current', 'page'); else a.removeAttribute('aria-current'); });
    var p = document.querySelector('.d-ctx-page'); if (p) p.textContent = getTitles()[v] || v;
    // tabs: the stage of the page shown is lit and lists its pages; a page under More opens that group and leaves the stage as it is
    var a = document.querySelector('.d-pages .d-rail-link.active');
    if (a) { var g = a.closest('.d-pg-grp'); if (g) { stageShow(g.getAttribute('data-stage')); stageLast[stageCur] = v; }
             var m = a.closest('.d-pg-more'); if (m) m.open = true; }
    stripPlace(v);
  }
  // tabs (CS): the stage switch on the top bar, each stage's last page used (the first page the first time)
  var stageCur = null, stageLast = {};
  function stageShow(i){ stageCur = String(i);
    document.querySelectorAll('.d-stage-btn').forEach(function(b){ b.setAttribute('aria-current', b.getAttribute('data-stage') === stageCur ? 'true' : 'false'); });
    document.querySelectorAll('.d-pg-grp').forEach(function(g){ g.hidden = g.getAttribute('data-stage') !== stageCur; }); stageMarks(); }
  document.addEventListener('click', function(e){ var b = e.target.closest('.d-stage-btn'); if (!b) return; var i = b.getAttribute('data-stage');
    var f = document.querySelector('.d-pg-grp[data-stage="' + i + '"] .d-rail-link'); stageShow(i);
    var v = stageLast[i] || (f && f.getAttribute('data-ovid-go')); if (v) go(v); });
  // a stage is done (a tick) when its steps' status lines are all ok or info, one at least ok; the counts of its steps show beside it
  function stageMarks(){ document.querySelectorAll('.d-stage-btn').forEach(function(b){ var i = b.getAttribute('data-stage'), n = 0, lv = [];
    document.querySelectorAll('.d-pg-grp[data-stage="' + i + '"] .d-step-st').forEach(function(s){ n += +(s.getAttribute('data-count') || 0);
      var l = ['ok', 'info', 'warn', 'error', 'off'].filter(function(c){ return s.classList.contains(c); })[0]; if (l && s.textContent) lv.push(l); });
    var done = lv.length && lv.indexOf('ok') >= 0 && lv.every(function(l){ return l === 'ok' || l === 'info'; });
    b.classList.toggle('done', !!done); var k = b.querySelector('.d-stg-k'); if (k) k.textContent = done && i !== stageCur ? '✓' : k.getAttribute('data-n');
    var c = b.querySelector('.d-stg-cnt'); if (c) { c.textContent = n > 999 ? '999+' : n > 0 ? n : ''; c.title = n > 0 ? n + ' waiting' : ''; } }); }
  // the strip (e.g. the stations) shows at the top of the pages that use it, under the page's heading
  function stripPlace(v){ var s = document.querySelector('.d-strip[data-pages]'); if (!s) return;
    var on = s.getAttribute('data-pages').split('|').indexOf(v) >= 0, t = on && document.querySelector('.d-work > .tabbable > .tab-content > .tab-pane[data-value="' + CSS.escape(v) + '"] > .d-tool');
    s.hidden = !t; if (!t) return;
    var h = t.firstElementChild; while (h && h.tagName === 'SCRIPT') h = h.nextElementSibling;
    if (h && (h.matches('.d-head, h2, .d-step-head') || h.querySelector('h2'))) h.after(s); else t.prepend(s); resize(); }
  // Go to (tabs): every page with its stage, every station on the strip, Settings; Cmd/Ctrl+K opens it, arrows choose, Enter opens, Esc closes
  var gotoHits = [], gotoSel = 0;
  function gotoItems(){ var it = [], seen = {}, t = getTitles(), s = document.querySelector('.d-strip[data-pages]');
    document.querySelectorAll('.d-pages .d-rail-link[data-ovid-go]').forEach(function(a){ var v = a.getAttribute('data-ovid-go'); seen[v] = 1;
      var g = a.closest('.d-pg-grp'); it.push({ t: t[v] || v, k: g ? g.getAttribute('data-name') : 'More', go: function(){ go(v); } }); });
    Object.keys(t).forEach(function(v){ if (!seen[v]) it.push({ t: t[v], k: '', go: function(){ go(v); } }); });
    if (s) { var to = s.getAttribute('data-pages').split('|')[0];
      s.querySelectorAll('button').forEach(function(b){ var n = b.textContent.trim(); if (n) it.push({ t: n, k: 'Station', go: function(){ b.click(); if (to) go(to); } }); }); }
    return it; }
  function gotoList(){ var pal = document.querySelector('.d-goto-pal'), q = pal.querySelector('.d-goto-in').value.trim().toLowerCase(), box = pal.querySelector('.d-goto-list');
    gotoHits = gotoItems().filter(function(i){ return !q || i.t.toLowerCase().indexOf(q) >= 0 || i.k.toLowerCase().indexOf(q) >= 0; }).slice(0, 14);
    gotoSel = Math.max(0, Math.min(gotoSel, gotoHits.length - 1)); box.textContent = '';
    if (!gotoHits.length) { var e = document.createElement('div'); e.className = 'd-goto-none'; e.textContent = 'Nothing by that name.'; box.appendChild(e); }
    gotoHits.forEach(function(h, i){ var b = document.createElement('button'); b.type = 'button'; b.className = 'd-goto-item' + (i === gotoSel ? ' sel' : '');
      b.id = 'd-goto-' + i; b.setAttribute('role', 'option'); b.setAttribute('aria-selected', i === gotoSel ? 'true' : 'false'); b.setAttribute('data-i', i); b.tabIndex = -1;
      var a = document.createElement('span'); a.textContent = h.t; var k = document.createElement('small'); k.textContent = h.k; b.appendChild(a); b.appendChild(k); box.appendChild(b); });
    var sel = box.querySelector('.sel'); if (sel) sel.scrollIntoView({ block: 'nearest' });
    pal.querySelector('.d-goto-in').setAttribute('aria-activedescendant', gotoHits.length ? 'd-goto-' + gotoSel : ''); }
  function gotoOpen(){ var pal = document.querySelector('.d-goto-pal'); if (!pal) return; pal.hidden = false; var i = pal.querySelector('.d-goto-in'); i.value = ''; gotoSel = 0; gotoList(); i.focus(); }
  function gotoClose(back){ var pal = document.querySelector('.d-goto-pal'); if (!pal || pal.hidden) return; pal.hidden = true; var b = document.querySelector('.d-goto'); if (back && b) b.focus(); }
  function gotoPick(h){ gotoClose(false); if (h) h.go(); }
  window.ovidGoto = gotoOpen;
  document.addEventListener('mouseover', function(e){ var r = e.target.closest && e.target.closest('.d-run-st'); if (r) r.title = r.textContent.trim(); });   // the run state cut short reads in full
  document.addEventListener('click', function(e){ if (e.target.closest('.d-goto')) { gotoOpen(); return; }
    var it = e.target.closest('.d-goto-item'); if (it) { gotoPick(gotoHits[+it.getAttribute('data-i')]); return; }
    if (e.target.classList && e.target.classList.contains('d-goto-pal')) gotoClose(true); });
  document.addEventListener('input', function(e){ if (e.target.classList && e.target.classList.contains('d-goto-in')) { gotoSel = 0; gotoList(); } });
  document.addEventListener('keydown', function(e){ var pal = document.querySelector('.d-goto-pal'); if (!pal) return;
    if ((e.metaKey || e.ctrlKey) && !e.altKey && e.key.toLowerCase() === 'k') { e.preventDefault(); if (pal.hidden) gotoOpen(); else gotoClose(true); return; }
    if (pal.hidden) return;
    if (e.key === 'Escape') { e.preventDefault(); e.stopPropagation(); gotoClose(true); }
    else if (e.key === 'ArrowDown') { e.preventDefault(); gotoSel = Math.min(gotoSel + 1, gotoHits.length - 1); gotoList(); }
    else if (e.key === 'ArrowUp') { e.preventDefault(); gotoSel = Math.max(gotoSel - 1, 0); gotoList(); }
    else if (e.key === 'Enter') { e.preventDefault(); gotoPick(gotoHits[gotoSel]); }
    else if (e.key === 'Tab') e.preventDefault(); }, true);
  document.addEventListener('DOMContentLoaded', function(){ var k = document.querySelector('.d-goto kbd');
    if (k && !/Mac|iPhone|iPad/.test(navigator.platform || '')) k.textContent = 'Ctrl K'; });
  function focusIcon(box, on){ var i = box.querySelector('.d-focus-btn i'); if (i) { i.classList.toggle('fa-expand', !on); i.classList.toggle('fa-compress', on); } }
  function focusOff(){ document.querySelectorAll('.d-focused').forEach(function(f){ f.classList.remove('d-focused'); focusIcon(f, false); }); document.body.classList.remove('d-has-focus'); resize(); }
  // an open menu closes on a click outside it, or on Esc
  document.addEventListener('mousedown', function(e){ document.querySelectorAll('details.d-pop[open]').forEach(function(d){ if (!d.contains(e.target)) d.removeAttribute('open'); }); });
  document.addEventListener('keydown', function(e){ if (e.key === 'Escape') document.querySelectorAll('details.d-pop[open]').forEach(function(d){ d.removeAttribute('open'); }); });
  document.addEventListener('click', function(e){
    var g = e.target.closest('[data-ovid-go]'); if (g) { e.preventDefault(); go(g.getAttribute('data-ovid-go'));
      document.querySelectorAll('details.d-pop[open]').forEach(function(d){ d.removeAttribute('open'); }); return; }
    var f = e.target.closest('.d-focus-btn');
    if (f) { var box = f.closest('.d-focusable'); var on = !box.classList.contains('d-focused'); focusOff();
             if (on) { box.classList.add('d-focused'); focusIcon(box, true); document.body.classList.add('d-has-focus'); resize(); } return; }
    var s = e.target.closest('.d-split-btn');
    if (s) { var sp = document.getElementById(s.getAttribute('data-target')); if (sp) { sp.classList.toggle('on'); s.classList.toggle('active', sp.classList.contains('on')); resize(); } return; }
    if (e.target.closest('.d-rail-toggle')) { document.body.classList.toggle('d-rail-min');
      try { localStorage.setItem('ovid-rail-min', document.body.classList.contains('d-rail-min') ? '1' : '0'); } catch(_) {} resize(); return; }
    document.querySelectorAll('details.d-pop[open]').forEach(function(d){ if (!d.contains(e.target)) d.removeAttribute('open'); });
  });
  document.addEventListener('keydown', function(e){ if (e.key === 'Escape') { focusOff(); document.querySelectorAll('details.d-pop[open]').forEach(function(d){ d.removeAttribute('open'); }); } });
  // No browser autofill in any typed field (names, notes, editor cells, table filters, fields drawn later too): Safari
  // offered names from the Mac's Contacts in a site-name box. autocomplete=off covers Chrome, Edge and Firefox; Safari
  // ignores it for Contacts but never fills a field whose name holds 'search' (Shiny reads the id, not the name).
  var NOFILL = 'input:not([type]),input[type=text],input[type=search],input[type=email],input[type=tel],input[type=url],input[type=number],textarea';
  function noFill(el){
    if (el.hasAttribute('data-d-nofill')) return; el.setAttribute('data-d-nofill', '');
    el.setAttribute('autocomplete', 'off'); el.setAttribute('autocorrect', 'off'); el.setAttribute('autocapitalize', 'off');
    el.setAttribute('data-1p-ignore', ''); el.setAttribute('data-lpignore', 'true'); el.setAttribute('data-form-type', 'other');
    if (!el.getAttribute('name')) el.setAttribute('name', 'search-' + (el.id || 'field'));
  }
  // What the field offers instead comes from the app (suggest_server): the last values typed in the same field, and names from
  // the set-up tables and deployment sheet in any field whose label names that kind (site, station, serial).
  var SUG = {};
  function sugOk(el){ return el.tagName === 'INPUT' && el.id && !/-selectized$/.test(el.id) && !el.closest('.selectize-control'); }
  function sugLabel(el){ var l = document.querySelector('label[for="' + CSS.escape(el.id) + '"]'); return ((l ? l.textContent : '') + ' ' + (el.placeholder || '')).toLowerCase(); }
  function sugFor(el){
    var v = [], seen = {}, lab = sugLabel(el);
    function add(a){ (a || []).forEach(function(x){ x = String(x); var k = x.toLowerCase(); if (x && !seen[k]) { seen[k] = 1; v.push(x); } }); }
    add(SUG['id:' + el.id + ':typed']); add(SUG['id:' + el.id + ':names']);
    var w = Object.keys(SUG).filter(function(k){ return k.indexOf('word:') === 0 && new RegExp('\\b' + k.slice(5) + 's?\\b').test(lab); });
    w.filter(function(k){ return !w.some(function(o){ return o !== k && o.indexOf(k.slice(5)) >= 0; }); }).forEach(function(k){ add(SUG[k]); });   // 'site group' over 'site'
    return v;
  }
  function sugApply(el){
    if (!sugOk(el)) return; var v = sugFor(el), lid = 'd-sug-' + el.id, dl = document.getElementById(lid);
    if (!v.length) { if (dl) dl.textContent = ''; return; }
    if (!dl) { dl = document.createElement('datalist'); dl.id = lid; document.body.appendChild(dl); }
    dl.textContent = ''; v.slice(0, 200).forEach(function(x){ var o = document.createElement('option'); o.value = x; dl.appendChild(o); });
    el.setAttribute('list', lid);
  }
  function sugAll(){ document.querySelectorAll(NOFILL).forEach(sugApply); }
  function noFillIn(root){ if (root.nodeType !== 1) return; if (root.matches(NOFILL)) { noFill(root); sugApply(root); } root.querySelectorAll(NOFILL).forEach(function(el){ noFill(el); sugApply(el); }); }
  document.addEventListener('change', function(e){ var el = e.target;
    if (!el.matches || !el.matches(NOFILL) || !sugOk(el) || !window.Shiny || !Shiny.setInputValue) return;
    var x = String(el.value || '').trim(); if (!x || x.length > 200) return;
    var k = 'id:' + el.id + ':typed', a = (SUG[k] || []).filter(function(y){ return String(y).toLowerCase() !== x.toLowerCase(); });
    SUG[k] = [x].concat(a).slice(0, 15); sugApply(el);
    Shiny.setInputValue('ovid_typed', { id: el.id, value: x }, { priority: 'event' });
  });
  document.addEventListener('DOMContentLoaded', function(){
    noFillIn(document.body);
    new MutationObserver(function(ms){ ms.forEach(function(m){ m.addedNodes.forEach(noFillIn); }); })
      .observe(document.body, { childList: true, subtree: true });
    $(function(){ if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler('ovid-suggest', function(m){
      SUG[m.word ? 'word:' + String(m.word).toLowerCase().replace(/[^a-z0-9 ]/g, '') : 'id:' + m.id + ':' + (m.set || 'typed')] = m.values || []; sugAll(); }); });
  });
  // the top bar never overflows (shell.css): the corrections chip is cut short first; once cut, it shows its short form, whole
  function fitBar(){ document.querySelectorAll('.d-applied').forEach(function(c){ var s = c.querySelector(':scope>summary'); if (!s) return;
    c.classList.remove('d-short'); if (s.scrollWidth > s.clientWidth + 1) c.classList.add('d-short'); }); }
  var fitT = null; function fitSoon(){ clearTimeout(fitT); fitT = setTimeout(fitBar, 60); }
  window.addEventListener('resize', fitSoon);
  $(document).on('shiny:value', function(e){ if (/(^|-)chip$/.test(e.name)) setTimeout(fitBar, 0); });
  document.addEventListener('DOMContentLoaded', function(){
    var bar = document.querySelector('.d-top'); if (bar && window.MutationObserver) new MutationObserver(fitSoon).observe(bar, { childList: true, subtree: true, characterData: true });
    try { if (localStorage.getItem('ovid-rail-min') === '1') document.body.classList.add('d-rail-min'); } catch(_) {}
    var a = document.querySelector('#nav li.active a, #nav a.active'); if (a) mark(a.getAttribute('data-value'));
  });
  // the flow: step status lines, the Explanations switch (remembered), the Workbench link with this page's selection
  var MARK = {ok: '✓', warn: '▲', error: '▲', info: '○', off: '○'};
  document.addEventListener('DOMContentLoaded', function(){
    $(function(){ if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler('ovid-steps', function(m){
      Object.keys(m).forEach(function(k){ document.querySelectorAll('.d-step-st[data-step="' + CSS.escape(k) + '"]').forEach(function(el){
        el.className = 'd-step-st ' + m[k].level; el.setAttribute('data-count', m[k].count || 0);
        el.textContent = m[k].text ? (el.closest('.d-pages') ? '' : (MARK[m[k].level] || '') + ' ') + m[k].text : ''; }); }); stageMarks(); }); });
    var ex = document.getElementById('d_explain'); if (ex) { var off = false; try { off = localStorage.getItem('ovid-explain') === '0'; } catch(_) {}
      ex.checked = !off; document.body.classList.toggle('d-quiet', off);
      ex.addEventListener('change', function(){ document.body.classList.toggle('d-quiet', !ex.checked); try { localStorage.setItem('ovid-explain', ex.checked ? '1' : '0'); } catch(_) {} }); }
  });
  document.addEventListener('DOMContentLoaded', function(){
    $(document).on('shown.bs.tab', '#nav a', function(e){ mark(e.target.getAttribute('data-value')); });
    $(document).on('shiny:inputchanged', function(e){ if (e.name === 'nav') mark(e.value); });
  });
})();

