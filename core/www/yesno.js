// Yes/no cells in the Standard set-up table editor (core editor.R): a double-click opens a Yes / No pick in place of
// typing; the pick is sent as the table's cell edit, as a typed value would be. The columns come from the server
// (ovid-yesno: the table's output id and its yes/no columns, from 0).
(function(){ var YN = {};
  function reg(){ if (!window.Shiny || !Shiny.addCustomMessageHandler || reg.on) return; reg.on = true;
    Shiny.addCustomMessageHandler('ovid-yesno', function(m){ YN[m.id] = [].concat(m.cols || []); }); }
  reg(); $(document).on('shiny:connected', reg);
  $(document).on('dblclick', 'td', function(e){ var box = $(this).closest('.datatables'), id = box.attr('id'); if (!id || !YN[id] || !YN[id].length) return;
    var api = $(this).closest('table').DataTable(), cell = api.cell(this), ix = cell.index(); if (!ix || YN[id].indexOf(ix.column) < 0) return;
    e.preventDefault(); e.stopImmediatePropagation(); var td = this, now = String(cell.data() || '');
    var sel = $('<select class="form-select form-select-sm" aria-label="Yes or No"><option value="">(blank)</option><option>Yes</option><option>No</option></select>').val(['Yes', 'No'].indexOf(now) >= 0 ? now : '');
    $(td).empty().append(sel); sel.trigger('focus');
    function done(save){ var v = sel.val(); sel.off(); if (save && v !== now) { cell.data(v); Shiny.setInputValue(id + '_cell_edit', {row: ix.row + 1, col: ix.column, value: v}, {priority: 'event'}); }
      $(td).text(save ? v : now); }
    sel.on('change', function(){ done(true); }).on('blur', function(){ done(false); }).on('keydown', function(k){ if (k.key === 'Escape') done(false); }); });
})();
