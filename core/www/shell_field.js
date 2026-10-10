
(function(){ var on = false;
  function show(m){ var el = document.getElementById(m.id); if (!el) return; var box = el.closest('.shiny-input-container') || el.closest('.form-group') || el.parentNode;
    var e = box.querySelector(':scope > .d-field-err'); if (!e) { e = document.createElement('div'); e.className = 'd-field-err'; box.appendChild(e); }
    e.textContent = m.msg; box.classList.add('d-has-err'); try { el.focus(); } catch (x) {} }
  function reg(){ if (on || !window.Shiny || !Shiny.addCustomMessageHandler) return; on = true; Shiny.addCustomMessageHandler('ovid-field-err', show); }
  reg(); $(document).on('shiny:connected', reg);
  $(document).on('input change', '.d-has-err input, .d-has-err select, .d-has-err textarea', function(){ var box = this.closest('.d-has-err'); if (!box) return;
    box.classList.remove('d-has-err'); var e = box.querySelector(':scope > .d-field-err'); if (e) e.remove(); });
  // every table with five or more columns gets a Columns picker (show or hide columns; the table and its file keep them all).
  // The core table (ovid_dt, table.js) has its own Columns panel, which also keeps the choice, so it is skipped here.
  $(document).on('init.dt', function(e, settings){ try { var api = new $.fn.dataTable.Api(settings), n = api.columns().count(); if (n < 5 || $(api.table().node()).hasClass('d-tbl')) return;
    var w = $(api.table().container()); if (w.find('.d-colpick').length) return;
    var d = $('<details class="d-colpick"><summary class="btn btn-default btn-sm" title="Choose the columns to show">Columns</summary><div class="d-colpick-pop"></div></details>');
    var pop = d.find('.d-colpick-pop'); api.columns().every(function(i){ var h = $(this.header()).text().trim() || ('column ' + (i + 1));
      var l = $('<label><input type="checkbox"' + (this.visible() ? ' checked' : '') + '> </label>'); l.append(document.createTextNode(h));
      l.find('input').on('change', function(){ api.column(i).visible(this.checked); api.columns.adjust(); }); pop.append(l); });
    w.prepend(d); } catch (x) {} });
  // the Workbench's correction buttons wait for a selection, and say so; Correct a period also works on typed dates
  $(document).on('shiny:value', function(e){ if (e.name !== 'wb_grid') return; setTimeout(function(){ var v = Shiny.shinyapp.$inputValues['wb_grid_sel']; $(document).trigger($.Event('shiny:inputchanged', {name: 'wb_grid_sel', value: v})); }, 50); });
  $(document).on('shiny:inputchanged', function(e){ if (e.name !== 'wb_grid_sel') return; var none = !e.value || !e.value.length;
    document.querySelectorAll('.wb-acts button').forEach(function(b){ if (b.id === 'wb_act_period') return;
      if (none) { if (!b.hasAttribute('data-t0')) b.setAttribute('data-t0', b.title || ''); b.disabled = true; b.title = 'Select cells in the grid, or drag across the chart, first'; }
      else if (b.hasAttribute('data-t0')) { b.disabled = false; b.title = b.getAttribute('data-t0'); b.removeAttribute('data-t0'); } }); });
})();

