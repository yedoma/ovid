
(function(){
  function busy(m){ (m.ids || []).forEach(function(id){ var b = document.getElementById(id); if (!b) return;
    if (m.on) { if (!b.hasAttribute('data-d-busy')) { b.setAttribute('data-d-busy', b.getAttribute('title') || ''); b.disabled = true; b.setAttribute('title', m.why);
        if (b.tagName === 'A') { b.classList.add('disabled'); b.setAttribute('aria-disabled', 'true'); } } }   // a download link cannot be disabled: it is marked
    else if (b.hasAttribute('data-d-busy')) { b.disabled = false; var t = b.getAttribute('data-d-busy'); if (t) b.setAttribute('title', t); else b.removeAttribute('title'); b.removeAttribute('data-d-busy');
      if (b.tagName === 'A') { b.classList.remove('disabled'); b.removeAttribute('aria-disabled'); } } }); }
  $(function(){ if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler('ovid-busy', busy); });
})();

