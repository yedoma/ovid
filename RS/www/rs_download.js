// www/rs_download.js  --  Starts a download when the server says its file is ready (the shareable viewer, written in the
// background first). Part of Ovid Core // RS. The server sends session$sendCustomMessage("rs-download", "<download button id>").
(function () {
  $(function () {
    if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler('rs-download', function (id) {
      var a = document.getElementById(id); if (a) a.click();
    });
  });
})();
