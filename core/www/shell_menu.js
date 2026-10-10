
(function(){ var CTX = {}, on = false;
  function reg(){ if (on || !window.Shiny || !Shiny.addCustomMessageHandler) return; on = true; Shiny.addCustomMessageHandler('ovid-ctx', function(m){ CTX = m || {}; }); }
  reg(); $(document).on('shiny:connected', reg);
  $(document).on('click', 'a.d-to-wb, #wb_to_standard', function(e){ var toWb = this.classList.contains('d-to-wb');
    if (window.ovidSheetFlush && ovidSheetFlush()) { e.preventDefault(); var h = href(toWb); setTimeout(function(){ location.href = h; }, 500); return; }   // unapplied sheet changes reach the server first
    this.setAttribute('href', href(toWb)); });
  function href(toWb){ var u = new URLSearchParams(); u.set('ui', toWb ? 'workbench' : 'standard');
    if (CTX.key && CTX.value) u.set(CTX.key, CTX.value); if (CTX.params && CTX.params.length) u.set('params', [].concat(CTX.params).join(','));
    ['from', 'to', 't', 't1'].forEach(function(k){ if (CTX[k]) u.set(k, CTX[k]); }); return '?' + u.toString(); }
  window.ovidSwitchHref = href;
  // before the first run (output has_out false), in the notebook layout only the first stage is ready: the later steps are dimmed,
  // marked available after import, and still open when clicked
  $(document).on('shiny:value', function(e){ if (e.name !== 'has_out') return; var none = !e.value; document.body.classList.toggle('d-noout', none);
    if (!document.documentElement.classList.contains('d-layout-notebook')) return; var stage = 0;
    document.querySelectorAll('.d-rail > *').forEach(function(x){ if (x.classList.contains('d-stage')) stage++;
      if (x.classList.contains('d-rail-link')) { var later = none && stage > 1; x.classList.toggle('d-later', later); x.title = later ? (x.getAttribute('data-t0') || x.title) + ' (available after import)' : (x.getAttribute('data-t0') || x.title);
        if (!x.getAttribute('data-t0')) x.setAttribute('data-t0', x.title.replace(' (available after import)', '')); } }); });
  // a download shows that it is being made until the server has made it (ovid-dl-done, sent by core shell.R
  // downloadHandler()), then that it was saved. A second click on it meanwhile, or a click while a run is going
  // (aria-disabled, core runner.js), does nothing, so one click is one file.
  if (!window.dOnceClick) { window.dOnceClick = true;   // the Workbench page holds this script twice: bind and register once (Shiny refuses a second handler)
  var dlBusy = [];
  function dlDone(a){ var html = a.getAttribute('data-d-html'); a.removeAttribute('data-d-html'); clearTimeout(a._dTimer); a.classList.remove('d-dl-busy');
    a.innerHTML = '✓ Saved to Downloads'; a.classList.add('d-dl-done'); setTimeout(function(){ a.innerHTML = html; a.classList.remove('d-dl-done'); }, 2500); }
  $(document).on('click', 'a.shiny-download-link', function(e){ var a = this;
    if (a.classList.contains('d-dl-busy') || a.classList.contains('d-dl-done') || a.getAttribute('aria-disabled') === 'true') { e.preventDefault(); return; }
    a.setAttribute('data-d-html', a.innerHTML); a.classList.add('d-dl-busy'); a.innerHTML = '<i class="fa fa-spinner fa-spin"></i> Preparing…'; dlBusy.push(a);
    a._dTimer = setTimeout(function(){ var i = dlBusy.indexOf(a); if (i >= 0) dlBusy.splice(i, 1); dlDone(a); }, 600000); });   // no word from the server in 10 min
  $(function(){ if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler('ovid-dl-done', function(m){ var a = dlBusy.shift(); if (a) dlDone(a); }); });
  // a button marked d-once (a long action that runs in the window) is turned off when clicked, so a double click runs it
  // once; the server turns it back on when the action ends (core shell.R once_done())
  $(document).on('click', 'button.d-once', function(){ var b = this; setTimeout(function(){ b.disabled = true; b.classList.add('d-once-busy'); }, 0); });
  $(function(){ if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler('ovid-once-done', function(m){ var b = document.getElementById(m.id);
    if (b) { b.classList.remove('d-once-busy'); if (!b.hasAttribute('data-d-busy')) b.disabled = false; } }); }); }
  // a link to a page and a tab on it (data-wb-open, as in the Workbench) in the Standard view
  $(document).on('click', '[data-wb-open]', function(e){ if (window.wbOpen) return; e.preventDefault(); var m = JSON.parse(this.getAttribute('data-wb-open'));
    if (window.ovidGo) ovidGo(m.go); setTimeout(function(){ var pane = document.querySelector('.tab-pane[data-value=\'' + m.go + '\']'); if (pane && m.tab) { var a = pane.querySelector('a[data-value=\'' + m.tab + '\']'); if (a) a.click(); } }, 120); });
})();

