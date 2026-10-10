// The sides of one app (ERT | GPR; core shell.R ovid_areas_ui): only the side's own pages show in the rail, the tabs, the
// Workbench tabs and tree (a page in no side, such as Reference, shows on every side); each side keeps its last page;
// ?area=<name> picks a side, Alt-1 / Alt-2 ... switch, a [data-area-go] button switches; the server sends the counts and the
// side to open ("ovid-areas"), and hears the side the person picks (input d_area, kept in the project's settings).
(function(){
  var el = document.getElementById('d-areas'); if (!el) return;
  var M = JSON.parse(el.textContent), names = Object.keys(M), cur = null, last = {}, proj = null, fromUrl = false, picked = false;
  function sideOf(v){ for (var i = 0; i < names.length; i++) if (M[names[i]].indexOf(v) >= 0) return names[i]; return null; }
  function goOf(e){ var g = e.getAttribute('data-ovid-go'); if (g) return g; try { return JSON.parse(e.getAttribute('data-wb-open')).go; } catch(_) { return null; } }
  function page(){ var a = document.querySelector('#nav a.active, #nav li.active > a'); return a ? a.getAttribute('data-value') : null; }
  function apply(){ if (!cur) return;
    document.querySelectorAll('.d-rail [data-ovid-go], .d-tabs [data-ovid-go], .wb-nav [data-ovid-go], #wb_tabbar [data-ovid-go], #wb_tabbar [data-wb-open]').forEach(function(e){
      var s = sideOf(goOf(e)); var off = !!s && s !== cur; var t = e.closest('.wb-tab') || e; t.classList.toggle('d-area-off', off); });
    document.querySelectorAll('.d-rail .d-rail-sec, .d-tabs .d-rail-sec').forEach(function(h){ var n = h.nextElementSibling, any = false;   // a label with nothing under it
      for (; n && !n.classList.contains('d-rail-sec'); n = n.nextElementSibling) if (n.matches('[data-ovid-go]') && !n.classList.contains('d-area-off')) any = true;
      h.classList.toggle('d-area-off', !any); });
    document.querySelectorAll('.d-area').forEach(function(a){ var on = a.getAttribute('data-area') === cur; a.classList.toggle('on', on); a.setAttribute('aria-pressed', on); });
    document.documentElement.setAttribute('data-area', cur); }
  function show(a, user){ if (!M[a]) return; var v = page();
    if (cur && v && sideOf(v) === cur) last[cur] = v;
    cur = a; apply(); v = page();
    if (sideOf(v) !== a && (user || (v && sideOf(v) !== null))) { var t = last[a] || M[a][0]; if (window.ovidGo) window.ovidGo(t); }
    if (user) { picked = true; if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('d_area', a, {priority: 'event'}); } }
  var q = (new URLSearchParams(location.search).get('area') || '').toLowerCase();
  names.forEach(function(a){ if (a.toLowerCase() === q) { fromUrl = true; cur = a; } }); if (!cur) cur = names[0];
  $(function(){ apply(); if (fromUrl) setTimeout(function(){ show(cur, false); }, 0);
    if (window.Shiny) Shiny.addCustomMessageHandler('ovid-areas', function(m){
      names.forEach(function(a){ var n = (m.counts || {})[a]; document.querySelectorAll('.d-area[data-area="' + a + '"]').forEach(function(e){
        var s = e.querySelector('.d-area-n'); if (s) s.textContent = n == null ? '' : Number(n).toLocaleString(); e.classList.toggle('d-area-dim', n === 0); }); });
      if (m.project !== proj) { var first = proj === null; proj = m.project; if (!fromUrl && !(first && picked) && m.area && M[m.area]) show(m.area, false); } }); });   // a side picked before the first word from the server stays
  $(document).on('click', '.d-area, [data-area-go]', function(e){ e.preventDefault(); show(this.getAttribute('data-area-go') || this.getAttribute('data-area'), true); });
  $(document).on('shiny:inputchanged', function(e){ if (e.name !== 'nav') return; var s = sideOf(e.value);   // a link to a page of the other side switches
    if (s && s !== cur) { cur = s; apply(); if (window.Shiny) Shiny.setInputValue('d_area', s, {priority: 'event'}); }
    if (s) last[s] = e.value; });
  $(document).on('shiny:value', function(){ setTimeout(apply, 0); });                                   // tabs drawn again (Workbench tab bar)
  document.addEventListener('keydown', function(e){ if (!e.altKey || e.ctrlKey || e.metaKey || !/^Digit[1-9]$/.test(e.code)) return;
    var a = names[+e.code.slice(5) - 1]; if (a) { e.preventDefault(); show(a, true); } });
})();
