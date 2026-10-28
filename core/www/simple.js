// The Simple view (simple.R): the four step buttons switch the hidden tabset simple_step; sp-done marks finished steps;
// an item chip dragged onto a group card sends {item, from, to} to the Edit module's drop input.
(function () {
  function show(step) {
    var a = document.querySelector('#simple_step a[data-value="' + step + '"]') ||
            document.querySelector('.sp-body .nav a[data-value="' + step + '"]');
    if (a) { if (window.jQuery) jQuery(a).tab('show'); else a.click(); }
    document.querySelectorAll('.sp-step').forEach(function (b) {
      var on = b.getAttribute('data-step') === step;
      b.classList.toggle('on', on); b.setAttribute('aria-selected', on ? 'true' : 'false');
    });
  }
  document.addEventListener('click', function (e) {
    var b = e.target.closest && e.target.closest('.sp-step[data-step]');
    if (b) { e.preventDefault(); show(b.getAttribute('data-step')); }
  });
  // a server-side updateTabsetPanel (or the first draw) keeps the buttons in step
  if (window.jQuery) jQuery(document).on('shown.bs.tab', '#simple_step a, .sp-body .nav a', function (e) {
    var v = e.target.getAttribute('data-value');
    document.querySelectorAll('.sp-step').forEach(function (b) { b.classList.toggle('on', b.getAttribute('data-step') === v); });
  });
  document.addEventListener('DOMContentLoaded', function () {
    var a = document.querySelector('#simple_step li.active a, #simple_step a.active');
    show(a ? a.getAttribute('data-value') : 'import');
  });
  function onShiny() {
    Shiny.addCustomMessageHandler('sp-done', function (d) {
      Object.keys(d || {}).forEach(function (k) {
        var b = document.querySelector('.sp-step[data-step="' + k + '"]'); if (b) b.classList.toggle('done', !!d[k]);
      });
    });
  }
  if (window.Shiny && Shiny.addCustomMessageHandler) onShiny(); else document.addEventListener('shiny:connected', onShiny, { once: true });

  // drag and drop of items onto groups
  var drag = null;
  document.addEventListener('dragstart', function (e) {
    var c = e.target.closest && e.target.closest('.sp-chip[data-item]'); if (!c) return;
    drag = { item: c.getAttribute('data-item'), from: c.getAttribute('data-from') || '' };
    c.classList.add('sp-dragging');
    try { e.dataTransfer.setData('text/plain', drag.item); e.dataTransfer.effectAllowed = 'move'; } catch (x) {}
  });
  document.addEventListener('dragend', function (e) {
    var c = e.target.closest && e.target.closest('.sp-chip'); if (c) c.classList.remove('sp-dragging');
    document.querySelectorAll('.sp-group.sp-over').forEach(function (g) { g.classList.remove('sp-over'); });
    drag = null;
  });
  document.addEventListener('dragover', function (e) {
    if (!drag) return; var g = e.target.closest && e.target.closest('.sp-group[data-group]'); if (!g) return;
    e.preventDefault(); g.classList.add('sp-over');
  });
  document.addEventListener('dragleave', function (e) {
    var g = e.target.closest && e.target.closest('.sp-group[data-group]');
    if (g && !g.contains(e.relatedTarget)) g.classList.remove('sp-over');
  });
  document.addEventListener('drop', function (e) {
    if (!drag) return; var g = e.target.closest && e.target.closest('.sp-group[data-group]'); if (!g) return;
    e.preventDefault(); g.classList.remove('sp-over');
    var to = g.getAttribute('data-group') || '', ed = g.closest('.sp-edit[data-ns]');
    if (to === drag.from || !ed || !window.Shiny) return;
    Shiny.setInputValue(ed.getAttribute('data-ns') + 'drop', { item: drag.item, from: drag.from, to: to, nonce: Date.now() }, { priority: 'event' });
  });
})();
