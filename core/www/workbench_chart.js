
// The Workbench chart: one panel per parameter on a shared time axis, drawn on a canvas in the browser.
// Gestures are the same on every ovid plot (_hub/Plot_table_standards.md A11, M 14 Oct): a drag left to right zooms to that
// stretch, right to left zooms out; Shift (or Cmd) + drag across a panel selects that stretch of its parameter in the grid,
// a click the record nearest the pointer, Shift + click adds to the selection, Esc clears.
// A bar above the selection gives its size, Zoom to this and the page's own actions. The wheel zooms at the pointer, a
// little per notch (sideways or shift-wheel scrolls); dragging the time axis, or holding Space and dragging, moves along;
// double-click shows the whole period and Back (Backspace) returns to the previous view. The grid's selection is
// highlighted here.
// Opened from Look closer in the Standard view (?t=2024-02-21T03:00&t1=...&p=parameter), the first chart shows two days
// around that time and selects those records.
// Data arrive as Shiny messages "wb-chart": type "base" (the period, thinned to the low and high of each stretch when it
// is long) and type "detail" (a zoomed-in window at full resolution, asked for through input wb_view).
(function(){
  'use strict';
  var MON = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
  function pad(n){ return (n < 10 ? '0' : '') + n; }
  function cssv(n, d){ var v = getComputedStyle(document.documentElement).getPropertyValue(n); return v && v.trim() ? v.trim() : d; }
  function lower(a, x){ var lo = 0, hi = a.length; while (lo < hi) { var m = (lo + hi) >> 1; if (a[m] < x) lo = m + 1; else hi = m; } return lo; }
  function arr(x){ return x == null ? [] : (Array.isArray(x) ? x : [x]); }
  // A stretch with no logger in place ("not deployed", M 14 Oct): grey diagonal hatching across the panel, kept apart from
  // the solid shading of removed and out-of-water stretches; the label goes on the first panel when there is room.
  function hatch(c, x0, x1, y, h, col, label, font){ var a = Math.max(x0, -10), b = x1; if (b - a < 1) return; c.save(); c.beginPath(); c.rect(a, y, b - a, h); c.clip();
    c.strokeStyle = col; c.globalAlpha = 0.4; c.lineWidth = 1.2; c.beginPath(); for (var k = Math.floor((a - h) / 7) * 7; k < b; k += 7) { c.moveTo(k, y + h); c.lineTo(k + h, y); } c.stroke();
    if (label && b - a > 90) { c.globalAlpha = 0.95; c.fillStyle = col; c.font = font; c.textAlign = 'center'; c.textBaseline = 'top'; var w = c.measureText(label).width + 10, m = (a + b) / 2;
      c.globalCompositeOperation = 'destination-out'; c.fillRect(m - w / 2, y + 4, w, 15); c.globalCompositeOperation = 'source-over'; c.fillText(label, m, y + 6); }
    c.restore(); }
  function niceY(lo, hi, n){ var span = hi - lo; if (!(span > 0)) { var a = Math.abs(lo) || 1; lo -= a / 2; hi += a / 2; span = hi - lo; }
    var raw = span / n, mag = Math.pow(10, Math.floor(Math.log10(raw))), f = raw / mag, s = (f < 1.5 ? 1 : f < 3 ? 2 : f < 7 ? 5 : 10) * mag, out = [];
    for (var v = Math.ceil(lo / s) * s; v <= hi + s * 1e-9; v += s) out.push(Math.abs(v) < s * 1e-9 ? 0 : v);
    return {ticks: out, digits: Math.max(0, Math.min(6, -Math.floor(Math.log10(s) + 1e-9)))}; }
  var STEPS = [60, 300, 900, 1800, 3600, 10800, 21600, 43200, 86400, 172800, 604800, 1209600], MONTH = 2629746;
  function timeTicks(v0, v1, w){ var span = v1 - v0, want = Math.max(2, Math.floor(w / 105)), out = [], i, t;
    for (i = 0; i < STEPS.length; i++) if (span / STEPS[i] <= want) { var s = STEPS[i]; for (t = Math.ceil(v0 / s) * s; t <= v1; t += s) out.push(t); return {ticks: out, step: s}; }
    var MS = [1, 2, 3, 6, 12, 24, 60, 120];
    for (i = 0; i < MS.length; i++) if (span / (MS[i] * MONTH) <= want || i === MS.length - 1) { var m = MS[i], d = new Date(v0 * 1000), y = d.getUTCFullYear(), mo = Math.floor(d.getUTCMonth() / m) * m;
      for (var guard = 0; guard < 400; guard++) { t = Date.UTC(y, mo, 1) / 1000; if (t > v1) break; if (t >= v0) out.push(t); mo += m; } return {ticks: out, step: m * MONTH}; }
    return {ticks: out, step: span}; }
  function tlab(t, step, first){ var d = new Date(t * 1000), day = MON[d.getUTCMonth()] + ' ' + pad(d.getUTCDate());
    if (step >= MONTH * 11.9) return '' + d.getUTCFullYear();
    if (step >= MONTH * 0.99) return MON[d.getUTCMonth()] + (d.getUTCMonth() === 0 || first ? ' ' + d.getUTCFullYear() : '');
    if (step >= 86400) return day + (first ? ' ' + d.getUTCFullYear() : '');
    var hm = pad(d.getUTCHours()) + ':' + pad(d.getUTCMinutes());
    return first || (d.getUTCHours() === 0 && d.getUTCMinutes() === 0) ? day + ' ' + hm : hm; }
  function ttxt(t){ var d = new Date(t * 1000); return d.getUTCFullYear() + '-' + pad(d.getUTCMonth() + 1) + '-' + pad(d.getUTCDate()) + ' ' + pad(d.getUTCHours()) + ':' + pad(d.getUTCMinutes()); }
  function vtxt(v){ if (v == null || !isFinite(v)) return '\u2014'; var a = Math.abs(v); return (v < 0 ? '\u2212' : '') + (a >= 1000 ? a.toFixed(0) : a >= 100 ? a.toFixed(1) : a >= 10 ? a.toFixed(2) : a.toFixed(3)); }
  function esc(s){ return String(s).replace(/[&<>"]/g, function(c){ return {'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;'}[c]; }); }
  // a time in the address (logger time, as the chart's numbers are): 2024-02-21T03:00 or 2024-02-21 03:00
  function qtime(x){ var m = /^(\d{4})-(\d\d)-(\d\d)(?:[T ](\d\d):(\d\d))?/.exec(x || ''); return m ? Date.UTC(+m[1], +m[2] - 1, +m[3], +(m[4] || 0), +(m[5] || 0)) / 1000 : null; }
  function goto(){ var q = new URLSearchParams(location.search), a = qtime(q.get('t')); if (a === null) return null; var b = qtime(q.get('t1'));
    return {t0: a, t1: b === null || b < a ? a : b, p: q.get('p')}; }

  function Chart(el){
    var self = this; this.el = el; this.cv = el.querySelector('canvas'); this.tip = el.querySelector('.wb-ctip'); this.ctx = this.cv.getContext('2d');
    this.base = null; this.detail = null; this.view = null; this.sel = []; this.active = null; this.hover = null; this.drag = null; this.hist = [];
    this.L = 104; this.R = 12; this.T = 6; this.B = 38; this.G = 8; this.W = 0; this.H = 0; this.k = 0; this.timer = null; this.go = goto();
    if (window.ResizeObserver) new ResizeObserver(function(){ self.resize(); }).observe(el);
    this.cv.addEventListener('mousedown', function(e){ self.down(e); });
    window.addEventListener('mousemove', function(e){ self.move(e); });
    window.addEventListener('mouseup', function(e){ self.up(e); });
    this.cv.addEventListener('mouseleave', function(){ if (!self.drag && self.hover) { self.hover = null; self.draw(); } });
    this.cv.addEventListener('wheel', function(e){ self.wheel(e); }, {passive: false});
    this.cv.addEventListener('dblclick', function(e){ e.preventDefault(); self.mark(); self.reset(); });
    this.act = document.createElement('div'); this.act.className = 'wb-cact'; this.act.hidden = true; el.appendChild(this.act);
    this.cv.addEventListener('contextmenu', function(e){ e.preventDefault(); });
    document.addEventListener('wb-grid-sel', function(e){ self.sel = e.detail.cells || []; self.active = e.detail.active; self.draw(); self.drawAct(); });
    this.resize();
  }
  var P = Chart.prototype;
  P.resize = function(){ var r = this.el.getBoundingClientRect(), d = window.devicePixelRatio || 1; if (r.width < 10 || r.height < 10) return;
    this.W = r.width; this.H = r.height; this.cv.width = Math.round(r.width * d); this.cv.height = Math.round(r.height * d);
    this.cv.style.width = r.width + 'px'; this.cv.style.height = r.height + 'px'; this.ctx.setTransform(d, 0, 0, d, 0, 0); this.draw(); };
  P.setBase = function(m){ m.spans = arr(m.spans); m.series = arr(m.series); m.rem = arr(m.rem); m.pend = arr(m.pend); m.flags = arr(m.flags); m.period = arr(m.period);
    var keep = !!(this.base && this.view && this.base.key === m.key && this.base.period[0] === m.period[0] && this.base.period[1] === m.period[1]);
    this.base = m; if (!keep) { this.view = m.period.slice(); this.detail = null; } else if (this.detail && this.detail.sig !== m.sig) this.detail = null;
    var g = this.go; if (g && m.series.length && g.t1 >= m.period[0] && g.t0 <= m.period[1]) { this.go = null; keep = false;
      this.view = [g.t0 - 86400, g.t1 + 86400]; this.active = {t: g.t0, p: g.p || m.series[0].p};
      var p = this.active.p; setTimeout(function(){ if (window.wbGridSelectRange) window.wbGridSelectRange(p, g.t0 - 1, g.t1 + 1, false); }, 900); }
    this.clamp(); this.draw(); this.request(keep); };
  P.setDetail = function(m){ if (!this.base || m.sig !== this.base.sig) return; m.series = arr(m.series); m.rem = arr(m.rem); m.pend = arr(m.pend); m.flags = arr(m.flags);
    m.cover = arr(m.cover); this.detail = m; this.draw(); };
  P.minWidth = function(){ var s = this.base && this.base.step > 0 ? this.base.step : 60; return 4 * s; };
  P.clamp = function(){ if (!this.base || !this.view) return; var p = this.base.period, full = p[1] - p[0], w = this.view[1] - this.view[0];
    w = Math.max(Math.min(w, full), Math.min(this.minWidth(), full)); var a = Math.max(p[0], Math.min(this.view[0], p[1] - w)); this.view = [a, a + w]; };
  P.set = function(a, b){ if (!this.base) return; this.view = [Math.min(a, b), Math.max(a, b)]; this.clamp(); this.draw(); this.request(); if (!this.drag) this.drawAct(); };
  P.reset = function(){ if (this.base) this.set(this.base.period[0], this.base.period[1]); };
  P.zoomBy = function(f, at){ if (!this.view) return; var c = at == null ? (this.view[0] + this.view[1]) / 2 : at; this.set(c - (c - this.view[0]) * f, c + (this.view[1] - c) * f); };
  // the view so far, for Back (Backspace)
  P.mark = function(){ if (this.view) { this.hist.push(this.view.slice()); if (this.hist.length > 30) this.hist.shift(); } };
  P.goBack = function(){ var v = this.hist.pop(); if (!v) return false; this.set(v[0], v[1]); return true; };
  P.selSpan = function(){ if (!this.sel.length) return null; var a = Infinity, b = -Infinity, rs = {}; this.sel.forEach(function(x){ if (x.t < a) a = x.t; if (x.t > b) b = x.t; rs[x.r] = 1; }); return {t0: a, t1: b, n: this.sel.length, rows: Object.keys(rs).length}; };
  P.zoomSel = function(){ var q = this.selSpan(); if (!q) return; var s = this.base && this.base.step > 0 ? this.base.step : 1800, pad = Math.max((q.t1 - q.t0) * 0.15, s * 4); this.mark(); this.set(q.t0 - pad, q.t1 + pad); };
  // the bar above the selection: how much is selected, Zoom to this, and the page's own actions (copies of its buttons)
  P.drawAct = function(){ var b = this.act, q = this.selSpan(); if (!q || this.drag || !this.view || !this.base) { b.hidden = true; return; }
    var acts = document.querySelectorAll('.wb-cellbar .wb-acts button:not([disabled])'), h = '<span class="n">' + q.n + (q.n === 1 ? ' value' : ' values') + (q.rows !== q.n ? ' in ' + q.rows + ' records' : '') + ' \u00b7 ' + ttxt(q.t0) + (q.t1 > q.t0 ? ' to ' + ttxt(q.t1) : '') + (this.base.zone ? ' ' + esc(this.base.zone) : '') + '</span>' +
      '<button type="button" data-wb-cact="zoom">Zoom to this</button>';
    acts.forEach(function(a, i){ if (a.offsetParent !== null && a.id) h += '<button type="button" data-wb-cact="' + esc(a.id) + '"' + (a.classList.contains('danger') ? ' class="danger"' : '') + '>' + esc(a.textContent.replace(/\s+[A-Z]$/, '').trim()) + '</button>'; });
    b.innerHTML = h + '<button type="button" data-wb-cact="clear" title="Esc">Clear</button>'; b.hidden = false;
    var pn = this.panels()[0], W = this.el.clientWidth, x = pn ? pn.x + (((q.t0 + q.t1) / 2) - this.view[0]) / (this.view[1] - this.view[0]) * pn.w : W / 2, bw = b.offsetWidth;
    b.style.left = Math.max(0, Math.min(W - bw, x - bw / 2)) + 'px'; };
  // tell the server the view (it sends detail when the period was thinned) and keep the grid's rows beside it
  P.request = function(quiet){ var self = this; clearTimeout(this.timer); this.timer = setTimeout(function(){ if (!self.view || !window.Shiny || !Shiny.setInputValue) return;
      Shiny.setInputValue('wb_view', {x0: self.view[0], x1: self.view[1], k: ++self.k}); if (!quiet && window.wbGridScrollTo) window.wbGridScrollTo(self.view[0]); }, 220); };
  // the data of a parameter in view: the zoomed-in detail where it covers the view, else the period
  P.data = function(kind, p){ var d = this.detail, v = this.view, src = d && d.cover[0] <= v[0] && d.cover[1] >= v[1] ? d : this.base; if (!src) return null;
    var a = src[kind]; for (var i = 0; i < a.length; i++) if (a[i].p === p) return {t: arr(a[i].t), v: arr(a[i].v), s: arr(a[i].s), e: arr(a[i].e), gap: src.gap || 0}; return null; };
  P.meta = function(p){ var s = this.base ? this.base.series : []; for (var i = 0; i < s.length; i++) if (s[i].p === p) return s[i]; return null; };
  P.panels = function(){ var n = this.base ? this.base.series.length : 0, out = []; if (!n) return out; var h = (this.H - this.T - this.B - this.G * (n - 1)) / n;
    for (var i = 0; i < n; i++) out.push({p: this.base.series[i].p, x: this.L, y: this.T + i * (h + this.G), w: this.W - this.L - this.R, h: h}); return out; };
  P.X = function(t, pn){ return pn.x + (t - this.view[0]) / (this.view[1] - this.view[0]) * pn.w; };
  P.T2 = function(x, pn){ return this.view[0] + (x - pn.x) / pn.w * (this.view[1] - this.view[0]); };
  P.panelAt = function(y){ var ps = this.panels(); for (var i = 0; i < ps.length; i++) if (y >= ps[i].y - this.G / 2 && y <= ps[i].y + ps[i].h + this.G / 2) return ps[i]; return null; };
  P.yr = function(p){ var lo = Infinity, hi = -Infinity, v0 = this.view[0], v1 = this.view[1];
    ['series', 'rem'].forEach(function(k){ var d = this.data(k, p); if (!d) return;
      for (var i = lower(d.t, v0); i < d.t.length && d.t[i] <= v1; i++) { var y = d.v[i]; if (y != null && isFinite(y)) { if (y < lo) lo = y; if (y > hi) hi = y; } } }, this);
    if (!isFinite(lo)) { lo = 0; hi = 1; } var pd = (hi - lo) * 0.06 || Math.abs(hi) * 0.05 || 0.5; return [lo - pd, hi + pd]; };
  P.draw = function(){ var c = this.ctx, W = this.W, H = this.H; if (!W || !H) return;
    var C = {bg: cssv('--wb-bg', '#171a15'), fg: cssv('--wb-fg', '#fbfcf6'), fg2: cssv('--wb-fg2', '#c0c7b7'), fg3: cssv('--wb-fg3', '#99a293'), grid: cssv('--wb-sel', '#363c34'),
             line: cssv('--wb-info', '#90dde4'), warn: cssv('--wb-warn', '#f7c775'), err: cssv('--wb-err', '#ea938b'), hl: cssv('--wb-acc2', '#ecf6ae'), mono: cssv('--wb-mono', 'monospace'), sans: cssv('--wb-title', 'sans-serif'), tick: cssv('--wb-line', '#858d80')};
    c.clearRect(0, 0, W, H); var ps = this.panels();
    if (!ps.length || !this.view) { c.fillStyle = C.fg3; c.font = '12px ' + C.mono; c.textBaseline = 'top'; c.fillText('Tick a parameter on the left to chart it.', this.L, 12); return; }
    var self = this, v0 = this.view[0], v1 = this.view[1], tk = timeTicks(v0, v1, ps[0].w), half = (this.base.step || 0) / 2;
    ps.forEach(function(pn){ var yr = self.yr(pn.p), Y = function(y){ return pn.y + pn.h - (y - yr[0]) / (yr[1] - yr[0]) * pn.h; }, X = function(t){ return self.X(t, pn); };
      var yt = niceY(yr[0], yr[1], Math.max(2, Math.floor(pn.h / 34)));
      c.save(); c.beginPath(); c.rect(pn.x, pn.y, pn.w, pn.h); c.clip();
      c.strokeStyle = C.grid; c.lineWidth = 1;
      tk.ticks.forEach(function(t){ var x = Math.round(X(t)) + 0.5; c.beginPath(); c.moveTo(x, pn.y); c.lineTo(x, pn.y + pn.h); c.stroke(); });
      yt.ticks.forEach(function(y){ var yy = Math.round(Y(y)) + 0.5; c.beginPath(); c.moveTo(pn.x, yy); c.lineTo(pn.x + pn.w, yy); c.stroke(); });
      self.base.spans.forEach(function(q){ if (q.kind !== 'absent' || q.t1 < v0 || q.t0 > v1) return; hatch(c, X(q.t0), X(q.t1), pn.y, pn.h, C.fg3, pn === ps[0] ? 'not deployed' : null, '11px ' + C.sans); });
      var f = self.data('flags', pn.p); if (f) { c.globalAlpha = 0.16; c.fillStyle = C.warn;
        for (var i = 0; i < f.s.length; i++) if (f.e[i] >= v0 && f.s[i] <= v1) c.fillRect(X(f.s[i]), pn.y, Math.max(1, X(f.e[i]) - X(f.s[i])), pn.h); c.globalAlpha = 1; }
      var mine = self.sel.filter(function(x){ return x.p === pn.p; }).sort(function(a, b){ return a.r - b.r; });
      if (mine.length) { c.globalAlpha = 0.22; c.fillStyle = C.hl; var s0 = 0;
        for (var j = 1; j <= mine.length; j++) if (j === mine.length || mine[j].r !== mine[j - 1].r + 1) { var a = X(mine[s0].t - half), b = X(mine[j - 1].t + half); c.fillRect(a, pn.y, Math.max(2, b - a), pn.h); s0 = j; }
        c.globalAlpha = 1; }
      var mt = self.meta(pn.p); if (mt) [['lo', mt.lo], ['hi', mt.hi]].forEach(function(l){ var v = l[1]; if (v == null || !isFinite(v) || v < yr[0] || v > yr[1]) return;
        var ly = Math.round(Y(v)) + 0.5; c.strokeStyle = C.err; c.globalAlpha = 0.75; c.lineWidth = 1; c.setLineDash([5, 4]); c.beginPath(); c.moveTo(pn.x, ly); c.lineTo(pn.x + pn.w, ly); c.stroke(); c.setLineDash([]);
        c.fillStyle = C.err; c.font = '10px ' + C.mono; c.textAlign = 'right'; c.textBaseline = l[0] === 'hi' ? 'top' : 'bottom'; c.fillText((l[0] === 'hi' ? 'max ' : 'min ') + vtxt(v), pn.x + pn.w - 4, ly + (l[0] === 'hi' ? 2 : -2)); c.globalAlpha = 1; });
      var d = self.data('series', pn.p); if (d) { c.strokeStyle = C.line; c.lineWidth = 1.3; c.lineJoin = 'round'; c.beginPath(); self.path(c, d, X, Y, pn); c.stroke(); }
      var pe = self.data('pend', pn.p); if (pe) { c.strokeStyle = C.err; c.lineWidth = 1.2;
        for (var k = lower(pe.t, v0); k < pe.t.length && pe.t[k] <= v1; k++) { if (pe.v[k] == null) continue; c.beginPath(); c.arc(X(pe.t[k]), Y(pe.v[k]), 3.6, 0, 6.2832); c.stroke(); } }
      var rm = self.data('rem', pn.p); if (rm) { c.strokeStyle = C.err; c.lineWidth = 1.5;
        for (var q = lower(rm.t, v0); q < rm.t.length && rm.t[q] <= v1; q++) { if (rm.v[q] == null) continue; var rx = X(rm.t[q]), ry = Y(rm.v[q]);
          c.beginPath(); c.moveTo(rx - 3.5, ry - 3.5); c.lineTo(rx + 3.5, ry + 3.5); c.moveTo(rx + 3.5, ry - 3.5); c.lineTo(rx - 3.5, ry + 3.5); c.stroke(); } }
      if (mine.length) { c.fillStyle = C.hl; c.strokeStyle = C.bg; c.lineWidth = 1;
        mine.forEach(function(x){ if (x.v == null || x.t < v0 || x.t > v1) return; c.beginPath(); c.arc(X(x.t), Y(x.v), 3.4, 0, 6.2832); c.fill(); c.stroke(); }); }
      if (self.active && self.active.p === pn.p && self.active.t >= v0 && self.active.t <= v1) { var ax = Math.round(X(self.active.t)) + 0.5;
        c.strokeStyle = C.hl; c.setLineDash([3, 3]); c.beginPath(); c.moveTo(ax, pn.y); c.lineTo(ax, pn.y + pn.h); c.stroke(); c.setLineDash([]); }
      c.restore();
      c.strokeStyle = C.grid; c.lineWidth = 1; c.strokeRect(pn.x + 0.5, pn.y + 0.5, pn.w - 1, pn.h - 1);
      c.strokeStyle = C.tick; c.beginPath();
      yt.ticks.forEach(function(y){ var yy = Math.round(Y(y)) + 0.5; if (yy > pn.y && yy < pn.y + pn.h) { c.moveTo(pn.x - 4, yy); c.lineTo(pn.x, yy); } }); c.stroke();
      c.fillStyle = C.fg2; c.font = '10.5px ' + C.mono; c.textAlign = 'right'; c.textBaseline = 'middle';
      yt.ticks.forEach(function(y){ var yy = Y(y); if (yy > pn.y + 6 && yy < pn.y + pn.h - 4) c.fillText(y.toFixed(yt.digits).replace('-', '\u2212'), pn.x - 7, yy); });
      c.save(); c.translate(12, pn.y + pn.h / 2); c.rotate(-Math.PI / 2); c.textAlign = 'center'; c.fillStyle = C.fg; c.font = '500 11.5px ' + C.sans;
      var full = pn.p + (mt && mt.u ? ' (' + mt.u + ')' : ''), room = Math.floor((pn.h - 8) / 6.6), lab = full.length > room ? full.slice(0, Math.max(3, room - 1)) + '\u2026' : full; c.fillText(lab, 0, 0); c.restore(); });
    var last = ps[ps.length - 1], prev = -1e9; c.strokeStyle = C.tick; c.lineWidth = 1; c.beginPath();
    tk.ticks.forEach(function(t){ var x = Math.round(self.X(t, last)) + 0.5; if (x >= last.x && x <= last.x + last.w) { c.moveTo(x, last.y + last.h); c.lineTo(x, last.y + last.h + 4); } }); c.stroke();
    c.fillStyle = C.fg2; c.font = '10.5px ' + C.mono; c.textAlign = 'center'; c.textBaseline = 'top';
    tk.ticks.forEach(function(t, i){ var x = self.X(t, last), s = tlab(t, tk.step, i === 0), w = c.measureText(s).width; if (x - w / 2 < Math.max(prev + 8, 2) || x + w / 2 > W) return;
      c.fillText(s, x, last.y + last.h + 6); prev = x + w / 2; });
    c.font = '11px ' + C.sans; c.fillText('Time' + (this.base.zone ? ' (' + this.base.zone + ')' : ''), last.x + last.w / 2, last.y + last.h + 22);
    if (this.drag && this.drag.mode !== 'pan' && Math.abs(this.drag.x - this.drag.x0) > 3) {
      var x0 = Math.max(this.L, Math.min(this.drag.x0, this.drag.x)), x1 = Math.min(W - this.R, Math.max(this.drag.x0, this.drag.x)), sp = this.drag.mode === 'select' ? [this.drag.pn] : ps;
      c.globalAlpha = this.drag.mode === 'select' ? 0.22 : 0.1; c.fillStyle = this.drag.mode === 'select' ? C.hl : C.fg; sp.forEach(function(pn){ c.fillRect(x0, pn.y, x1 - x0, pn.h); });
      c.globalAlpha = 1; c.strokeStyle = C.hl; c.lineWidth = 1; sp.forEach(function(pn){ c.strokeRect(x0 + 0.5, pn.y + 0.5, Math.max(1, x1 - x0 - 1), pn.h - 1); }); }
    if (this.hover && !this.drag) { var hx = Math.round(this.hover.x) + 0.5; c.strokeStyle = C.fg3; c.lineWidth = 1; c.beginPath(); c.moveTo(hx, ps[0].y); c.lineTo(hx, last.y + last.h); c.stroke(); }
    this.tipShow(); };
  // the line, broken at missing values and at gaps; more points than pixels are drawn as the low and high of each column
  P.path = function(c, d, X, Y, pn){ var t = d.t, v = d.v, i0 = Math.max(0, lower(t, this.view[0]) - 1), i1 = Math.min(t.length, lower(t, this.view[1]) + 1), gap = d.gap > 0 ? d.gap : Infinity;
    var many = (i1 - i0) > pn.w * 2, pen = false, lt = null, cx = null, mn, mx, fi, la, pl = null;
    function flush(){ if (cx === null) return; if (pl !== null) { c.moveTo(cx - 1, Y(pl)); c.lineTo(cx, Y(fi)); } c.moveTo(cx + 0.5, Y(mn)); c.lineTo(cx + 0.5, Y(mx)); pl = la; cx = null; }
    for (var i = i0; i < i1; i++) { var y = v[i], brk = y == null || (lt !== null && t[i] - lt > gap); lt = t[i];
      if (many) { if (brk) { flush(); pl = null; if (y == null) continue; } var x = Math.round(X(t[i])); if (x !== cx) { flush(); cx = x; mn = mx = fi = la = y; } else { if (y < mn) mn = y; if (y > mx) mx = y; la = y; } }
      else { if (y == null) { pen = false; continue; } if (brk) pen = false; var px = X(t[i]), py = Y(y); if (!pen) { c.moveTo(px, py); pen = true; } else c.lineTo(px, py); } }
    if (many) flush(); };
  P.tipShow = function(){ var tip = this.tip; if (!tip) return; if (!this.hover || this.drag || !this.base) { tip.style.display = 'none'; return; }
    var ps = this.panels(), t = this.T2(this.hover.x, ps[0]), rows = [], tt = null;
    ps.forEach(function(pn){ var d = this.data('series', pn.p); if (!d || !d.t.length) return; var i = lower(d.t, t);
      if (i > 0 && (i >= d.t.length || Math.abs(d.t[i - 1] - t) < Math.abs(d.t[i] - t))) i--; if (tt === null) tt = d.t[i];
      var v = d.v[i], r = this.data('rem', pn.p), old = null; if (r) { var k = lower(r.t, d.t[i]); if (k < r.t.length && r.t[k] === d.t[i]) old = r.v[k]; }
      rows.push('<div><span>' + esc(pn.p) + '</span><b>' + (v == null && old != null ? '\u2715 ' + vtxt(old) : vtxt(v)) + '</b></div>'); }, this);
    var gap = this.base.spans.filter(function(q){ return q.kind === 'absent' && t >= q.t0 && t <= q.t1; })[0];
    if (gap) { tip.innerHTML = '<div class="h">' + esc(gap.label || 'Not deployed') + '</div>'; tt = undefined; }
    else if (tt === null) { tip.style.display = 'none'; return; }
    else tip.innerHTML = '<div class="h">' + ttxt(tt) + (this.base.zone ? ' ' + esc(this.base.zone) : '') + '</div>' + rows.join('');
    tip.style.display = 'block'; var x = this.hover.x + 14, w = tip.offsetWidth; if (x + w > this.W - 4) x = this.hover.x - w - 14;
    tip.style.left = Math.max(4, x) + 'px'; tip.style.top = Math.max(4, Math.min(this.hover.y - 10, this.H - tip.offsetHeight - 4)) + 'px'; };
  P.pos = function(e){ var r = this.cv.getBoundingClientRect(); return {x: e.clientX - r.left, y: e.clientY - r.top}; };
  P.onAxis = function(q){ return q.y > this.H - this.B; };
  P.down = function(e){ if ((e.button !== 0 && e.button !== 2) || !this.base || !this.view) return; var q = this.pos(e), ax = this.onAxis(q), pn = this.panelAt(q.y) || (ax ? this.panels()[0] : null); if (!pn || q.x < this.L - 4) return; e.preventDefault();
    var right = e.button === 2 || (e.ctrlKey && /Mac/.test(navigator.platform)), add = e.shiftKey || e.metaKey,
      m = !right && (ax || SPACE || e.altKey) ? 'pan' : (right || add) ? 'select' : 'zoom';      // plain drag zooms (M 14 Oct); Shift + drag selects
    if (m === 'pan') this.mark(); this.act.hidden = true;
    this.drag = {x0: q.x, x: q.x, mode: m, pn: pn, view: this.view.slice(), add: e.shiftKey || e.metaKey || (e.ctrlKey && !right), right: right}; this.cv.style.cursor = m === 'pan' ? 'grabbing' : 'crosshair'; };
  P.move = function(e){ var q = this.pos(e);
    if (this.drag) { this.drag.x = q.x; if (this.drag.mode === 'pan') { var pn = this.drag.pn, dv = (this.drag.x0 - q.x) / pn.w * (this.drag.view[1] - this.drag.view[0]);
        this.view = [this.drag.view[0] + dv, this.drag.view[1] + dv]; this.clamp(); } this.draw(); return; }
    if (e.target === this.cv) this.cv.style.cursor = SPACE || this.onAxis(q) ? 'grab' : 'crosshair';
    var inside = e.target === this.cv && q.x >= this.L && q.x <= this.W - this.R && q.y >= this.T && q.y <= this.H - this.B;
    if (!inside && !this.hover) return; this.hover = inside ? q : null; this.draw(); };
  P.up = function(e){ var d = this.drag; if (!d) return; this.drag = null; this.cv.style.cursor = ''; var pn = d.pn;
    if (d.mode === 'pan') { if (Math.abs(d.x - d.x0) > 3) this.request(); else this.hist.pop(); this.draw(); this.drawAct(); return; }
    if (Math.abs(d.x - d.x0) <= (d.mode === 'zoom' ? 5 : 3)) { if (d.right) { this.drawAct(); return; } if (window.wbGridSelectNear) window.wbGridSelectNear(pn.p, this.T2(d.x0, pn), d.add); this.draw(); return; }
    var a = this.T2(Math.max(this.L, Math.min(d.x0, d.x)), pn), b = this.T2(Math.min(this.W - this.R, Math.max(d.x0, d.x)), pn);
    if (d.mode === 'zoom') { this.mark(); if (d.x > d.x0) this.set(a, b);                         // left to right: zoom to the stretch
      else { var w = (this.view[1] - this.view[0]) * 2, c = (a + b) / 2; this.set(c - w / 2, c + w / 2); } return; }   // right to left: twice as wide
    if (window.wbGridSelectRange) window.wbGridSelectRange(pn.p, a, b, d.add); this.draw(); };
  P.wheel = function(e){ if (!this.base || !this.view) return; e.preventDefault(); var q = this.pos(e), pn = this.panelAt(q.y) || this.panels()[0]; if (!pn) return;
    var dy = e.deltaMode === 1 ? e.deltaY * 16 : e.deltaY, dx = e.deltaMode === 1 ? e.deltaX * 16 : e.deltaX, w = this.view[1] - this.view[0];
    if (e.shiftKey && !dx) { dx = dy; dy = 0; }
    if (Math.abs(dx) > Math.abs(dy)) { var s = dx / pn.w * w; this.set(this.view[0] + s, this.view[1] + s); return; }
    this.zoomBy(Math.min(1.2, Math.max(1 / 1.2, Math.exp(dy * 0.0015))), this.T2(Math.max(pn.x, Math.min(q.x, pn.x + pn.w)), pn)); };

  var chart = null, SPACE = false;
  function get(){ if (!chart) { var el = document.getElementById('wb_chart'); if (el) chart = window.wbChart = new Chart(el); } return chart; }
  $(function(){ get();
    if (window.Shiny && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler('wb-chart', function(m){ var c = get(); if (!c) return; if (m.type === 'detail') c.setDetail(m); else c.setBase(m); });
      Shiny.addCustomMessageHandler('wb-chart-view', function(m){ var c = get(); if (c) c.set(m.x0, m.x1); }); } });
  $(document).on('shown.bs.tab', function(){ if (chart) setTimeout(function(){ chart.resize(); }, 30); });
  $(document).on('shiny:inputchanged', function(e){ if (e.name === 'wb_layout' && chart) { chart.el.classList.toggle('tall', e.value === 'chart'); setTimeout(function(){ chart.resize(); }, 30); } });
  $(document).on('click', '[data-wb-chart]', function(e){ e.preventDefault(); var c = get(); if (!c || !c.view) return; var a = this.getAttribute('data-wb-chart'), w = c.view[1] - c.view[0];
    if (a !== 'period' && a !== 'back') c.mark();
    if (a === 'in') c.zoomBy(0.5); else if (a === 'out') c.zoomBy(2); else if (a === 'reset') c.reset(); else if (a === 'back') c.goBack();
    else if (a === 'left') c.set(c.view[0] - w / 2, c.view[1] - w / 2); else if (a === 'right') c.set(c.view[0] + w / 2, c.view[1] + w / 2);
    else if (a === 'period' && window.Shiny) Shiny.setInputValue('wb_view_period', {x0: c.view[0], x1: c.view[1], k: Date.now()}); });
  // Esc clears, Space held down makes a drag move the chart, Backspace over the chart goes back a view
  $(document).on('keydown', function(e){ if (e.key === 'Escape' && !$(e.target).is('input,textarea,select,.wb-gwrap,.wb-gwrap *') && window.wbGridClear) window.wbGridClear();
    if ($(e.target).is('input,textarea,select,[contenteditable]')) return; var c = chart;
    if (e.key === ' ' && c && c.el.matches(':hover')) { SPACE = true; e.preventDefault(); }
    else if (e.key === 'Backspace' && c && c.el.matches(':hover') && c.goBack()) e.preventDefault();
    // keys (plot standards A11): + and - zoom, arrows move the selection by a record (Shift extends it)
    else if (c && c.view && c.el.matches(':hover') && !e.metaKey && !e.ctrlKey && !e.altKey) {
      if (e.key === '+' || e.key === '=') { c.mark(); var q0 = c.selSpan(); c.zoomBy(1 / 1.5, q0 ? (q0.t0 + q0.t1) / 2 : null); e.preventDefault(); }
      else if (e.key === '-' || e.key === '_') { c.mark(); var q1 = c.selSpan(); c.zoomBy(1.5, q1 ? (q1.t0 + q1.t1) / 2 : null); e.preventDefault(); }
      else if ((e.key === 'ArrowLeft' || e.key === 'ArrowRight') && c.sel.length && window.wbGridSelectRange) {
        var q = c.selSpan(), st = c.base && c.base.step > 0 ? c.base.step : 1800, d = e.key === 'ArrowRight' ? st : -st, ps = {};
        c.sel.forEach(function(x){ ps[x.p] = 1; });
        var t0 = e.shiftKey ? (d < 0 ? q.t0 + d : q.t0) : q.t0 + d, t1 = e.shiftKey ? (d > 0 ? q.t1 + d : q.t1) : q.t1 + d;
        Object.keys(ps).forEach(function(p, i){ window.wbGridSelectRange(p, t0 - 1, t1 + 1, i > 0); });
        var w = c.view[1] - c.view[0], edge = d > 0 ? t1 : t0; if (edge < c.view[0] || edge > c.view[1]) c.set(edge - w / 2, edge + w / 2);
        e.preventDefault(); } } });
  $(document).on('keyup', function(e){ if (e.key === ' ') SPACE = false; });
  $(document).on('click', '[data-wb-cact]', function(e){ e.preventDefault(); var c = get(), a = this.getAttribute('data-wb-cact'); if (!c) return;
    if (a === 'zoom') c.zoomSel(); else if (a === 'clear') { if (window.wbGridClear) window.wbGridClear(); } else { var b = document.getElementById(a); if (b) b.click(); } });
})();

