// RS/www/rs_edits.js  --  The Save bar's keys: Cmd/Ctrl+S saves what waits in it, and leaving the page with changes not saved
// asks first. Part of Ovid Core // RS. The server sends session$sendCustomMessage("rs-unsaved", <count>) (pages/list_edits.R).
(function () {
  var unsaved = 0;
  function hook() {
    if (!(window.Shiny && Shiny.addCustomMessageHandler)) return setTimeout(hook, 200);
    Shiny.addCustomMessageHandler('rs-unsaved', function (n) { unsaved = n || 0; });
  }
  hook();
  document.addEventListener('keydown', function (e) {
    if (!(e.metaKey || e.ctrlKey) || e.shiftKey || e.altKey || (e.key !== 's' && e.key !== 'S')) return;
    if (!unsaved || (document.activeElement && document.activeElement.closest && document.activeElement.closest('.dsheet'))) return;   // the Workbench sheet saves its own
    e.preventDefault(); Shiny.setInputValue('sv_key', Date.now(), { priority: 'event' });
  });
  window.addEventListener('beforeunload', function (e) { if (unsaved > 0) { e.preventDefault(); e.returnValue = ''; } });
})();
