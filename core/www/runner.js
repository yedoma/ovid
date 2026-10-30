
(function(){
  function busy(m){ (m.ids || []).forEach(function(id){ var b = document.getElementById(id); if (!b) return;
    if (m.on) { if (!b.hasAttribute('data-d-busy')) { b.setAttribute('data-d-busy', b.getAttribute('title') || ''); b.disabled = true; b.setAttribute('title', m.why);
        if (b.tagName === 'A') { b.classList.add('disabled'); b.setAttribute('aria-disabled', 'true'); } } }   // a download link cannot be disabled: it is marked
    else if (b.hasAttribute('data-d-busy')) { b.disabled = false; var t = b.getAttribute('data-d-busy'); if (t) b.setAttribute('title', t); else b.removeAttribute('title'); b.removeAttribute('data-d-busy');
      if (b.tagName === 'A') { b.classList.remove('disabled'); b.removeAttribute('aria-disabled'); } } }); }
  // the progress box's clock, bar and lines, changed in place (runner.R run_server): the box itself is drawn once per run
  // (and any copy of it an app shows in its page, marked data-ovid-run)
  function run(m){ document.querySelectorAll('#ovid_runner .d-run, [data-ovid-run]').forEach(function(b){ var q = function(s){ return b.querySelector(s) || {style: {}}; };
    q('.d-run-time').textContent = m.time; q('.d-run-fill').style.width = m.pct + '%'; q('.d-run-stage').textContent = m.stage;
    var it = q('.d-run-item'); it.textContent = m.item; it.style.display = m.item ? '' : 'none'; }); }
  $(function(){ if (window.Shiny && Shiny.addCustomMessageHandler) { Shiny.addCustomMessageHandler('ovid-busy', busy); Shiny.addCustomMessageHandler('ovid-run', run); } });
})();

