
(function(){
  function resize(){ setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 80); }
  function go(v){ var a = document.querySelector('#nav a[data-value="' + CSS.escape(v) + '"]'); if (a) a.click(); }
  window.ovidGo = go;
  var titles = null;
  function mark(v){
    document.querySelectorAll('.d-rail-link').forEach(function(a){ a.classList.toggle('active', a.getAttribute('data-ovid-go') === v); });
    if (!titles) { try { titles = JSON.parse(document.getElementById('d-titles').textContent); } catch(e) { titles = {}; } }
    var p = document.querySelector('.d-ctx-page'); if (p) p.textContent = titles[v] || v;
  }
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
  document.addEventListener('DOMContentLoaded', function(){
    try { if (localStorage.getItem('ovid-rail-min') === '1') document.body.classList.add('d-rail-min'); } catch(_) {}
    var a = document.querySelector('#nav li.active a, #nav a.active'); if (a) mark(a.getAttribute('data-value'));
  });
  // the flow: step status lines, the Explanations switch (remembered), the Workbench link with this page's selection
  var MARK = {ok: '✓', warn: '▲', error: '▲', info: '○', off: '○'};
  document.addEventListener('DOMContentLoaded', function(){
    $(function(){ if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler('ovid-steps', function(m){
      Object.keys(m).forEach(function(k){ document.querySelectorAll('.d-step-st[data-step="' + CSS.escape(k) + '"]').forEach(function(el){
        el.className = 'd-step-st ' + m[k].level; el.textContent = m[k].text ? (MARK[m[k].level] || '') + ' ' + m[k].text : ''; }); }); }); });
    var ex = document.getElementById('d_explain'); if (ex) { var off = false; try { off = localStorage.getItem('ovid-explain') === '0'; } catch(_) {}
      ex.checked = !off; document.body.classList.toggle('d-quiet', off);
      ex.addEventListener('change', function(){ document.body.classList.toggle('d-quiet', !ex.checked); try { localStorage.setItem('ovid-explain', ex.checked ? '1' : '0'); } catch(_) {} }); }
  });
  document.addEventListener('DOMContentLoaded', function(){
    $(document).on('shown.bs.tab', '#nav a', function(e){ mark(e.target.getAttribute('data-value')); });
    $(document).on('shiny:inputchanged', function(e){ if (e.name === 'nav') mark(e.value); });
  });
})();

