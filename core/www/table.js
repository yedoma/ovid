// Ovid/www/table.js  --  the core table's behaviour (ovid_dt in table.R; plot and table standards A9).
//   Sorting comes from the button on each column (low to high, high to low, original order); clicking a header never sorts.
//   The same menu filters the column: a value range, only missing, or ticked values. Active filters show as chips.
//   Column widths drag from the header's right edge; double-click the edge to fit. Columns hides and shows columns.
//   Widths and hidden columns are kept per table in this browser, with Reset view.
//   Click a value, drag a block, or click a row number for the whole row; Shift extends, Ctrl/Cmd adds one. The footer gives
//   the count, mean, min, max and sum of the selected numbers; Copy (or Ctrl/Cmd+C) copies them as tab-separated text; Esc clears.
//   Values outside a quality limit are tinted, with the limit in the tooltip.
// Tables that keep DataTables' own row selection (selection = "single" or "multiple", used to pick a record for the page)
// keep it: they get the sort and filter menu, widths, Columns and the tints, but not value selection.
(function () {
  // Loaded in the page head, before DataTables itself arrives with the first table: the shared set-up runs on first use.
  if (window.ovidTable) return;
  var $ = null, ACTIVE = null, MENU = null, READY = false;

  function esc(s) { return String(s == null ? '' : s).replace(/[&<>"]/g, function (c) { return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]; }); }
  function load(k) { try { return JSON.parse(localStorage.getItem(k) || 'null'); } catch (e) { return null; } }
  function save(k, v) { try { localStorage.setItem(k, JSON.stringify(v)); } catch (e) { } }
  function num(v) { if (typeof v === 'number') return isFinite(v) ? v : null; if (v == null || v === '') return null; var x = Number(String(v).replace(/,/g, '')); return isFinite(x) ? x : null; }
  function isMissing(v) { return v == null || v === '' || v === 'NA' || (typeof v === 'number' && !isFinite(v)); }
  function fmt(x) { if (x == null) return '—'; var a = Math.abs(x); return a !== 0 && (a < 0.001 || a >= 1e6) ? x.toPrecision(4) : String(Math.round(x * 1000) / 1000); }
  function reEsc(s) { return String(s).replace(/[.*+?^${}()|[\]\\]/g, '\\$&'); }

  function closeMenu() { if (MENU) { MENU.remove(); MENU = null; } }
  function setup() {
    if (READY) return; READY = true; $ = window.jQuery;
    // Client-side tables filter here; server-side tables send the filter with the request (DT's range and regex searches).
    $.fn.dataTable.ext.search.push(function (settings, row, i) {
      var T = settings.nTable && settings.nTable.__ovid;
      return !T || T.server || T.pass(settings.aoData[i]._aData);
    });
    $(document).on('mousedown.ovidTable', function (e) { if (MENU && !$(e.target).closest('.d-menu,.d-sortb,.d-colsb').length) closeMenu(); });
    $(document).on('keydown.ovidTable', function (e) {
      if (e.key === 'Escape' && MENU) { closeMenu(); return; }
      if (!ACTIVE || /INPUT|SELECT|TEXTAREA/.test(e.target.tagName) || e.target.isContentEditable) return;
      if (!document.body.contains(ACTIVE.api.table().node())) { ACTIVE = null; return; }
      if (e.key === 'Escape' && Object.keys(ACTIVE.sel).length) { ACTIVE.clear(); }
      if ((e.metaKey || e.ctrlKey) && (e.key === 'c' || e.key === 'C') && Object.keys(ACTIVE.sel).length && !String(window.getSelection())) { e.preventDefault(); ACTIVE.copy(); }
    });
  }

  function init(api, settings) {
    setup();
    var node = api.table().node();
    if (node.__ovid) return;
    var o = settings.oInit.ovid || {};
    var wrap = $(api.table().container()), oid = wrap.closest('.datatables').attr('id') || '';
    var T = node.__ovid = {
      api: api, server: !!settings.oFeatures.bServerSide, o: o, names: o.names || [], types: o.types || [], levels: o.levels || {},
      limits: o.limits || {}, cells: !!o.cells, filt: {}, sel: {}, anchor: null, rowMode: false, key: 'ovid-tbl:' + location.pathname + ':' + oid,
      view: load('ovid-tbl:' + location.pathname + ':' + oid) || { w: {}, hid: [] }, oid: oid
    };
    T.view.w = T.view.w || {}; T.view.hid = T.view.hid || [];
    var cid = api.table().container().id, style = $('<style>').attr('data-ovid-w', cid).appendTo('head');
    $(node).addClass('d-tbl');
    function name(i) { return T.names[i] != null ? T.names[i] : String(i); }
    function label(i) { return $(settings.aoColumns[i].nTh).clone().children().remove().end().text().trim() || name(i); }

    // ---- widths ----
    T.applyW = function () {
      var css = '';
      api.columns().every(function (i) {
        var w = T.view.w[name(i)], k = api.column(i).index('visible');
        if (w && k != null) css += '#' + cid + ' th:nth-child(' + (k + 1) + '),#' + cid + ' td:nth-child(' + (k + 1) + '){width:' + w + 'px !important;min-width:' + w + 'px !important;max-width:' + w + 'px !important;overflow:hidden;text-overflow:ellipsis}';
      });
      style.text(css); api.columns.adjust();
    };
    T.store = function () { save(T.key, T.view); };

    // ---- header: the sort and filter button, the resize edge; a header click no longer sorts ----
    api.columns().every(function (i) {
      var th = $(settings.aoColumns[i].nTh);
      th.off('click.DT keypress.DT selectstart.DT').removeAttr('tabindex');
      if (!th.children('.d-sortb').length)
        th.append('<button type="button" class="d-sortb" data-c="' + i + '" aria-label="Sort and filter ' + esc(label(i)) + '" title="Sort and filter">▾</button><span class="d-rz" data-c="' + i + '" title="Drag to resize, double-click to fit"></span>');
    });
    wrap.on('mousedown.ovid', '.d-rz', function (e) {
      e.preventDefault(); e.stopPropagation();
      var h = $(this), i = +h.data('c'), th = h.parent(), x0 = e.clientX, w0 = th.outerWidth();
      h.addClass('on');
      $(document).on('mousemove.ovidRz', function (m) { T.view.w[name(i)] = Math.max(40, Math.round(w0 + m.clientX - x0)); T.applyW(); })
        .on('mouseup.ovidRz', function () { $(document).off('.ovidRz'); h.removeClass('on'); T.store(); });
    }).on('dblclick.ovid', '.d-rz', function (e) { e.stopPropagation(); delete T.view.w[name(+$(this).data('c'))]; T.applyW(); T.store(); })
      .on('click.ovid', '.d-sortb', function (e) { e.stopPropagation(); if (MENU && MENU.data('c') === +$(this).data('c') && MENU.data('t') === node) { closeMenu(); return; } menu(+$(this).data('c'), this); });

    function sortOf(i) { var r = null; api.order().forEach(function (x) { if (x[0] === i && r === null) r = x[1]; }); return r; }
    T.marks = function () {
      api.columns().every(function (i) {
        var b = $(settings.aoColumns[i].nTh).children('.d-sortb'), s = sortOf(i);
        b.toggleClass('on', !!s).toggleClass('f', !!T.filt[i]).text(s === 'asc' ? '↑' : s === 'desc' ? '↓' : '▾');
      });
    };

    // ---- the column menu: Sort, then Filter ----
    function menu(i, btn) {
      closeMenu();
      var t = T.types[i] || 'chr', isNum = t === 'num', isTime = t === 'time', s = sortOf(i), f = T.filt[i] || {};
      var lo = isNum ? 'Low to high' : isTime ? 'Earliest first' : 'A to Z', hi = isNum ? 'High to low' : isTime ? 'Latest first' : 'Z to A';
      var h = '<div class="h">Sort</div><button class="i' + (s === 'asc' ? ' on' : '') + '" data-o="asc">↑ ' + lo + '</button><button class="i' + (s === 'desc' ? ' on' : '') + '" data-o="desc">↓ ' + hi + '</button><button class="i" data-o="none">Original order</button><hr><div class="h">Filter</div>';
      var lv = T.levels[name(i)];
      if (isNum || (isTime && !T.server)) {
        h += '<div class="rng">from <input type="' + (isNum ? 'number' : 'text') + '" step="any" data-f="lo" value="' + esc(f.lo == null ? '' : f.lo) + '"' + (isTime ? ' placeholder="2024-07-01"' : '') + '> to <input type="' + (isNum ? 'number' : 'text') + '" step="any" data-f="hi" value="' + esc(f.hi == null ? '' : f.hi) + '"' + (isTime ? ' placeholder="2024-07-31"' : '') + '></div>';
        if (!T.server) h += '<label><input type="checkbox" data-f="miss"' + (f.miss ? ' checked' : '') + '> Only missing values</label>';
      } else if (lv && lv.length) {
        var on = f.vals || [];
        h += '<div class="vals">' + lv.map(function (v) { return '<label><input type="checkbox" data-v="' + esc(v) + '"' + (!f.vals || on.indexOf(String(v)) >= 0 ? ' checked' : '') + '> ' + esc(v === '' ? '(blank)' : v) + '</label>'; }).join('') + '</div>';
      } else if (isTime) {
        h += '<div class="note">Use the Filter box above the table to find times.</div>';
      } else {
        h += '<div class="rng"><input type="text" data-f="txt" placeholder="contains" value="' + esc(f.txt || '') + '" style="width:100%"></div>';
      }
      h += '<div class="acts"><button class="p" data-a="apply">Apply filter</button><button data-a="clear">Clear</button></div>';
      MENU = $('<div class="d-menu" role="menu">').html(h).data('c', i).data('t', node).appendTo('body');
      var r = btn.getBoundingClientRect(), mw = MENU.outerWidth();
      MENU.css({ top: Math.min(window.innerHeight - MENU.outerHeight() - 8, r.bottom + 4), left: Math.max(8, Math.min(window.innerWidth - mw - 8, r.right - mw)) });
      MENU.on('click', '[data-o]', function () {
        var k = $(this).data('o'); api.order(k === 'none' ? [] : [[i, k]]).draw(); closeMenu();
      }).on('click', '[data-a]', function () {
        if ($(this).data('a') === 'clear') { delete T.filt[i]; }
        else {
          var nf = {};
          MENU.find('[data-f]').each(function () { var k = $(this).data('f'); if (this.type === 'checkbox') { if (this.checked) nf[k] = true; } else if (this.value !== '') nf[k] = isNum ? Number(this.value) : this.value; });
          var boxes = MENU.find('[data-v]');
          if (boxes.length) { var vs = boxes.filter(':checked').map(function () { return String($(this).data('v')); }).get(); if (vs.length < boxes.length) nf.vals = vs; }
          if (Object.keys(nf).length) T.filt[i] = nf; else delete T.filt[i];
        }
        T.search(i); closeMenu();
      }).on('keydown', 'input', function (e) { if (e.key === 'Enter') MENU.find('[data-a=apply]').click(); });
      MENU.find('input,button').first().focus();
    }
    T.search = function (i) {
      if (T.server) {
        var f = T.filt[i], col = api.column(i);
        if (!f) col.search('');
        else if (f.vals) col.search(f.vals.length ? '^(' + f.vals.map(reEsc).join('|') + ')$' : '^\\b$', true, false);
        else if (f.txt != null) col.search(f.txt, false, true);
        else col.search((f.lo == null ? '' : f.lo) + ' ... ' + (f.hi == null ? '' : f.hi));
      }
      api.draw(); chips();
    };
    T.pass = function (row) {
      for (var i in T.filt) {
        var f = T.filt[i], v = row[i];
        if (f.miss) { if (!isMissing(v)) return false; continue; }
        if (f.vals) { if (f.vals.indexOf(v == null ? '' : String(v)) < 0) return false; continue; }
        if (f.txt != null) { if (String(v == null ? '' : v).toLowerCase().indexOf(String(f.txt).toLowerCase()) < 0) return false; continue; }
        if (T.types[i] === 'num') { var x = num(v); if (x == null || (f.lo != null && x < f.lo) || (f.hi != null && x > f.hi)) return false; }
        else { var sv = String(v == null ? '' : v); if (!sv || (f.lo != null && sv < String(f.lo)) || (f.hi != null && sv.slice(0, String(f.hi).length) > String(f.hi))) return false; }
      }
      return true;
    };
    function fdesc(i) {
      var f = T.filt[i];
      if (f.miss) return 'only missing';
      if (f.vals) return f.vals.length > 3 ? f.vals.length + ' values' : f.vals.map(function (v) { return v === '' ? '(blank)' : v; }).join(', ') || 'nothing';
      if (f.txt != null) return 'contains “' + f.txt + '”';
      return f.lo != null && f.hi != null ? f.lo + ' to ' + f.hi : f.lo != null ? '≥ ' + f.lo : '≤ ' + f.hi;
    }

    // ---- chips for active filters, and the Columns button ----
    var filterBox = wrap.find('.dataTables_filter');
    var chipsEl = $('<div class="d-chips">');
    if (filterBox.length) chipsEl.insertAfter(filterBox); else wrap.prepend(chipsEl);
    function chips() {
      chipsEl.html(Object.keys(T.filt).map(function (i) { return '<span class="d-chip">' + esc(label(+i)) + ': ' + esc(fdesc(i)) + '<button type="button" data-c="' + i + '" aria-label="Remove filter">×</button></span>'; }).join(''));
      T.marks();
    }
    chipsEl.on('click', 'button', function () { delete T.filt[$(this).data('c')]; T.search(+$(this).data('c')); });
    var colsB = $('<button type="button" class="d-colsb" title="Show, hide and reset columns">Columns</button>');
    if (api.columns().count() < 5) colsB = $();                                  // as the old picker: five or more columns
    else if (filterBox.length) filterBox.append(colsB); else chipsEl.before(colsB);
    colsB.on('click', function (e) {
      e.stopPropagation(); if (MENU && MENU.data('t') === 'cols' + cid) { closeMenu(); return; } closeMenu();
      var h = '<div class="h">Columns</div>';
      api.columns().every(function (i) { if (i === 0) return; h += '<label><input type="checkbox" data-k="' + i + '"' + (api.column(i).visible() ? ' checked' : '') + '> ' + esc(label(i)) + '</label>'; });
      h += '<hr><button class="i" data-a="reset">Reset view</button><div class="note">Kept for this table in this browser: shown columns and widths.</div>';
      MENU = $('<div class="d-menu">').html(h).data('t', 'cols' + cid).appendTo('body');
      var r = this.getBoundingClientRect(), mw = MENU.outerWidth();
      MENU.css({ top: r.bottom + 4, left: Math.max(8, Math.min(window.innerWidth - mw - 8, r.right - mw)) });
      MENU.on('change', '[data-k]', function () {
        var i = +$(this).data('k'), n = name(i); api.column(i).visible(this.checked, false);
        T.view.hid = T.view.hid.filter(function (x) { return x !== n; }); if (!this.checked) T.view.hid.push(n);
        T.store(); T.applyW(); T.clear();
      }).on('click', '[data-a=reset]', function () {
        T.view = { w: {}, hid: [] }; T.store(); api.columns().every(function (i) { api.column(i).visible(true, false); });
        T.filt = {}; api.columns().search(''); api.order([]).draw(); T.applyW(); chips(); closeMenu();
      });
    });

    // ---- saved view ----
    api.columns().every(function (i) { if (i > 0 && T.view.hid.indexOf(name(i)) >= 0) api.column(i).visible(false, false); });
    T.applyW();

    // ---- footer: selection statistics, Copy ----
    var foot = $('<div class="d-foot" aria-live="polite">');
    var info = wrap.find('.dataTables_info'); if (info.length) foot.insertBefore(info); else wrap.append(foot);
    T.clear = function () { T.sel = {}; T.anchor = null; T.paint(); };
    function rowsEls() { return $(node).children('tbody').children('tr'); }
    function cellEl(r, c) { var tr = rowsEls().get(r); return tr ? tr.cells[c] : null; }
    function cellText(td) { return td ? $(td).clone().children('.d-rn').remove().end().text().trim() : ''; }
    T.paint = function () {
      $(node).find('td.d-on').removeClass('d-on'); rowsEls().removeClass('d-rowsel');
      var keys = Object.keys(T.sel), rows = {}, nums = [];
      keys.forEach(function (k) {
        var p = k.split(':'), r = +p[0], c = +p[1], td = cellEl(r, c); if (!td) return; $(td).addClass('d-on'); rows[r] = (rows[r] || 0) + 1;
        var ci = api.column.index('fromVisible', c), d = ci == null ? null : api.cell(td).data();
        if (T.types[ci] === 'num') { var x = num(d); if (x != null) nums.push(x); }
      });
      var nvis = api.columns(':visible').count();
      Object.keys(rows).forEach(function (r) { if (rows[r] >= nvis) $(rowsEls().get(+r)).addClass('d-rowsel'); });
      if (!keys.length) { foot.html(''); return; }
      var nr = Object.keys(rows).length, h = '<span class="s">' + keys.length + (keys.length === 1 ? ' value' : ' values') + ' in ' + nr + (nr === 1 ? ' row' : ' rows') + '</span>';
      if (nums.length) {
        var sum = nums.reduce(function (a, b) { return a + b; }, 0);
        h += '<span>of the numbers: mean ' + fmt(sum / nums.length) + ', min ' + fmt(Math.min.apply(null, nums)) + ', max ' + fmt(Math.max.apply(null, nums)) + ', sum ' + fmt(sum) + '</span>';
      }
      foot.html(h + '<button type="button" data-a="copy" title="Ctrl/Cmd+C">Copy</button><button type="button" data-a="clear" title="Esc">Clear</button>');
      if (window.Shiny && Shiny.setInputValue && T.oid) {
        var rr = Object.keys(rows).map(Number).sort(function (a, b) { return a - b; });
        Shiny.setInputValue(T.oid + '_ovid_sel', { rows: T.server ? null : rr.map(function (r) { return api.row(rowsEls().get(r)).index() + 1; }), n: keys.length }, { priority: 'event' });
      }
    };
    T.copy = function () {
      var keys = Object.keys(T.sel); if (!keys.length) return;
      var rs = {}, cs = {};
      keys.forEach(function (k) { var p = k.split(':'); rs[p[0]] = 1; cs[p[1]] = 1; });
      var R = Object.keys(rs).map(Number).sort(function (a, b) { return a - b; }), C = Object.keys(cs).map(Number).sort(function (a, b) { return a - b; });
      var txt = R.map(function (r) { return C.map(function (c) { return T.sel[r + ':' + c] ? cellText(cellEl(r, c)).replace(/^—$/, '') : ''; }).join('\t'); }).join('\n');
      if (navigator.clipboard && navigator.clipboard.writeText) navigator.clipboard.writeText(txt).catch(function () { fallback(txt); }); else fallback(txt);
      foot.find('[data-a=copy]').text('Copied');
    };
    function fallback(txt) { var ta = $('<textarea style="position:fixed;left:-9999px">').val(txt).appendTo('body'); ta[0].select(); try { document.execCommand('copy'); } catch (e) { } ta.remove(); }
    foot.on('click', '[data-a=copy]', T.copy).on('click', '[data-a=clear]', function () { T.clear(); });

    // ---- value, block and row selection ----
    if (T.cells) {
      $(node).addClass('d-cells');
      var drag = null;
      function rect(a, b, set) {
        var r0 = Math.min(a[0], b[0]), r1 = Math.max(a[0], b[0]), c0 = Math.min(a[1], b[1]), c1 = Math.max(a[1], b[1]);
        for (var r = r0; r <= r1; r++) for (var c = c0; c <= c1; c++) set[r + ':' + c] = 1;
      }
      $(node).children('tbody').on('mousedown.ovid', 'td', function (e) {
        if (e.button !== 0 || $(e.target).is('input,select,textarea,a,button')) return;
        ACTIVE = T; closeMenu();
        var td = this, r = rowsEls().index(td.parentNode), c = td.cellIndex, nc = td.parentNode.cells.length - 1, row = $(e.target).closest('.d-rn').length > 0;
        if (r < 0 || td.parentNode.cells.length === 1) return;
        e.preventDefault();
        var a = row ? [r, 0] : [r, c], b = row ? [r, nc] : [r, c];
        if (e.shiftKey && T.anchor) {
          var base = T.anchor; T.sel = {};
          if (T.rowMode) rect([base[0], 0], [r, nc], T.sel); else rect(base, [r, c], T.sel);
        } else if (e.metaKey || e.ctrlKey) {
          var add = {}; rect(a, b, add); var allOn = Object.keys(add).every(function (k) { return T.sel[k]; });
          Object.keys(add).forEach(function (k) { if (allOn) delete T.sel[k]; else T.sel[k] = 1; });
          T.anchor = [r, row ? 0 : c]; T.rowMode = row;
        } else {
          T.sel = {}; rect(a, b, T.sel); T.anchor = [r, row ? 0 : c]; T.rowMode = row;
          drag = { r: r, c: c, row: row, nc: nc };
        }
        T.paint();
      }).on('mouseover.ovid', 'td', function () {
        if (!drag) return; var r = rowsEls().index(this.parentNode); if (r < 0) return;
        T.sel = {}; if (drag.row) rect([drag.r, 0], [r, drag.nc], T.sel); else rect([drag.r, drag.c], [r, this.cellIndex], T.sel); T.paint();
      });
      $(document).on('mouseup.ovid' + cid, function () { drag = null; });
    }

    // ---- every draw: row numbers, tints, marks ----
    function onDraw() {
      var start = api.page.info().start;
      var first = settings.aoColumns.length ? $(settings.aoColumns[0].nTh) : $();
      if (T.cells && !first.children('.d-rn').length) first.prepend('<span class="d-rn" style="cursor:default">#</span>');
      api.rows({ page: 'current' }).every(function (r, t, pos) {
        var tr = $(this.node()), data = this.data(), td0 = tr.children('td').first();
        if (T.cells && td0.length && !td0.children('.d-rn').length) td0.prepend('<span class="d-rn" title="Select the row">' + (T.server ? start + pos + 1 : r + 1) + '</span>');
        Object.keys(T.limits).forEach(function (col) {
          var i = T.names.indexOf(col), k = i < 0 ? null : api.column(i).index('visible'); if (k == null) return;
          var L = T.limits[col], x = num(data[i]), td = tr.children('td').eq(k);
          var out = x != null && ((L.min != null && x < L.min) || (L.max != null && x > L.max));
          td.toggleClass('d-lim', out);
          if (out) td.attr('title', 'Outside the quality limit (' + (L.label || ((L.min != null ? 'min ' + L.min : '') + (L.min != null && L.max != null ? ', ' : '') + (L.max != null ? 'max ' + L.max : ''))) + ')');
        });
      });
      T.sel = {}; T.anchor = null; T.paint(); T.marks();
    }
    api.on('draw.ovid', onDraw);
    api.on('column-visibility.ovid', function () { T.applyW(); });
    onDraw(); chips();
  }

  window.ovidTable = { init: init };
})();
