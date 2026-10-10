
// The Workbench point chart (wb_xy_ui): points at x and y, coloured by a value, for records that are not a time series
// (the measurements of an ERT survey on their pseudosection, images at their positions). Selecting is the same on every
// ovid plot (_hub/Plot_table_standards.md A11): drag selects the points in a box in the grid, a click the point nearest
// the pointer, Shift (or Cmd) adds, Esc clears; a bar above the selection gives its size, Zoom to this and the page's own
// actions. The wheel zooms at the pointer, Space + drag (or alt-drag) moves, double-click shows every point and Back
// (Backspace) returns to the previous view. The grid's selection is highlighted here.
// Data arrive as the Shiny message "wb-xy" (wb_xy_message() in workbench.R); a point's id is its row's key in the grid.
(function(){
  'use strict';
  function cssv(n, d){ var v = getComputedStyle(document.documentElement).getPropertyValue(n); return v && v.trim() ? v.trim() : d; }
  function arr(x){ return x == null ? [] : (Array.isArray(x) ? x : [x]); }
  function esc(s){ return String(s).replace(/[&<>"]/g, function(c){ return {'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;'}[c]; }); }
  function nice(lo, hi, n){ var span = hi - lo; if (!(span > 0)) { var a = Math.abs(lo) || 1; lo -= a / 2; hi += a / 2; span = hi - lo; }
    var raw = span / n, mag = Math.pow(10, Math.floor(Math.log10(raw))), f = raw / mag, s = (f < 1.5 ? 1 : f < 3 ? 2 : f < 7 ? 5 : 10) * mag, out = [];
    for (var v = Math.ceil(lo / s) * s; v <= hi + s * 1e-9; v += s) out.push(Math.abs(v) < s * 1e-9 ? 0 : v);
    return {ticks: out, digits: Math.max(0, Math.min(6, -Math.floor(Math.log10(s) + 1e-9)))}; }
  function rgb(h){ h = String(h || '').replace('#', ''); if (h.length === 3) h = h[0] + h[0] + h[1] + h[1] + h[2] + h[2];
    var n = parseInt(h.slice(0, 6), 16); return isNaN(n) ? [128, 128, 128] : [(n >> 16) & 255, (n >> 8) & 255, n & 255]; }
  function vtxt(v){ if (v == null || !isFinite(v)) return '\u2014'; var a = Math.abs(v);
    return (v < 0 ? '\u2212' : '') + (a >= 1000 ? a.toFixed(0) : a >= 100 ? a.toFixed(1) : a >= 10 ? a.toFixed(2) : a.toFixed(3)); }
  var STATE = {flag: 'flagged', pend: 'saved \u00b7 run to apply', back: 'restored \u00b7 run to apply'};

  function XY(el){
    var self = this; this.el = el; this.cv = el.querySelector('canvas'); this.tip = el.querySelector('.wb-ctip'); this.ctx = this.cv.getContext('2d');
    this.d = null; this.view = null; this.full = null; this.sel = new Set(); this.active = null; this.hover = -1; this.drag = null; this.hist = [];
    this.L = 60; this.R = 78; this.T = 10; this.B = 38; this.W = 0; this.H = 0; this.cols = [];
    if (window.ResizeObserver) new ResizeObserver(function(){ self.resize(); }).observe(el);
    this.cv.addEventListener('mousedown', function(e){ self.down(e); });
    window.addEventListener('mousemove', function(e){ self.move(e); });
    window.addEventListener('mouseup', function(e){ self.up(e); });
    this.cv.addEventListener('mouseleave', function(){ if (!self.drag && self.hover >= 0) { self.hover = -1; self.draw(); } });
    this.cv.addEventListener('wheel', function(e){ self.wheel(e); }, {passive: false});
    this.cv.addEventListener('dblclick', function(e){ e.preventDefault(); self.mark(); self.reset(); });
    this.act = document.createElement('div'); this.act.className = 'wb-cact'; this.act.hidden = true; el.appendChild(this.act);
    this.cv.addEventListener('contextmenu', function(e){ e.preventDefault(); });
    document.addEventListener('wb-grid-sel', function(e){ var s = new Set(); (e.detail.cells || []).forEach(function(c){ s.add(c.t); });
      self.sel = s; self.active = e.detail.active ? e.detail.active.t : null; self.draw(); self.drawAct(); });
    this.resize();
  }
  var P = XY.prototype;
  P.resize = function(){ var r = this.el.getBoundingClientRect(), d = window.devicePixelRatio || 1; if (r.width < 10 || r.height < 10) return;
    this.W = r.width; this.H = r.height; this.cv.width = Math.round(r.width * d); this.cv.height = Math.round(r.height * d);
    this.cv.style.width = r.width + 'px'; this.cv.style.height = r.height + 'px'; this.ctx.setTransform(d, 0, 0, d, 0, 0);
    if (this.base0) { var whole = this.view && this.full && this.view.join() === this.full.join(); this.full = this.fit(this.base0); this.view = whole ? this.full.slice() : this.fit(this.view); }
    this.draw(); };
  // equal = true (positions in metres): a metre is as long across as up, so the view grows to the plot's shape
  P.fit = function(v){ if (!v || !this.d || !this.d.equal) return v; var pw = this.pw(), ph = this.ph(), s = Math.max((v[1] - v[0]) / pw, (v[3] - v[2]) / ph),
    cx = (v[0] + v[1]) / 2, cy = (v[2] + v[3]) / 2; return [cx - s * pw / 2, cx + s * pw / 2, cy - s * ph / 2, cy + s * ph / 2]; };
  P.setData = function(m){ ['x', 'y', 'id', 'c', 'v', 's', 'lab', 'clim', 'cticks', 'clabs', 'pal'].forEach(function(k){ m[k] = arr(m[k]); });
    var x0 = Infinity, x1 = -Infinity, y0 = Infinity, y1 = -Infinity;
    for (var i = 0; i < m.x.length; i++) { var x = m.x[i], y = m.y[i]; if (x == null || y == null || !isFinite(x) || !isFinite(y)) continue;
      if (x < x0) x0 = x; if (x > x1) x1 = x; if (y < y0) y0 = y; if (y > y1) y1 = y; }
    if (!isFinite(x0)) { x0 = 0; x1 = 1; y0 = 0; y1 = 1; }
    var px = (x1 - x0) * 0.04 || 0.5, py = (y1 - y0) * 0.06 || 0.5, base = [x0 - px, x1 + px, y0 - py, y1 + py];
    var keep = !!(this.d && this.view && this.d.key === m.key && this.base0 && this.base0.join() === base.join());
    this.d = m; this.base0 = base; this.full = this.fit(base); this.view = keep ? this.fit(this.view) : this.full.slice();
    this.cols = m.pal.map(rgb); this.hover = -1; this.draw(); };
  P.pw = function(){ return Math.max(10, this.W - this.L - this.R); };
  P.ph = function(){ return Math.max(10, this.H - this.T - this.B); };
  P.X = function(x){ return this.L + (x - this.view[0]) / (this.view[1] - this.view[0]) * this.pw(); };
  P.Y = function(y){ return this.T + (this.view[3] - y) / (this.view[3] - this.view[2]) * this.ph(); };
  P.ix = function(px){ return this.view[0] + (px - this.L) / this.pw() * (this.view[1] - this.view[0]); };
  P.iy = function(py){ return this.view[3] - (py - this.T) / this.ph() * (this.view[3] - this.view[2]); };
  P.set = function(v){ if (!this.full) return; v = this.fit(v); var f = this.full, mw = (f[1] - f[0]) * 1e-4, mh = (f[3] - f[2]) * 1e-4;
    if (v[1] - v[0] < mw || v[3] - v[2] < mh) return; this.view = v; this.draw(); if (!this.drag) this.drawAct(); };
  P.reset = function(){ if (this.full) { this.view = this.full.slice(); this.draw(); this.drawAct(); } };
  P.zoomBy = function(f, at){ if (!this.view) return; var v = this.view, cx = at ? at[0] : (v[0] + v[1]) / 2, cy = at ? at[1] : (v[2] + v[3]) / 2;
    this.set([cx - (cx - v[0]) * f, cx + (v[1] - cx) * f, cy - (cy - v[2]) * f, cy + (v[3] - cy) * f]); };
  P.mark = function(){ if (this.view) { this.hist.push(this.view.slice()); if (this.hist.length > 30) this.hist.shift(); } };
  P.goBack = function(){ var v = this.hist.pop(); if (!v) return false; this.set(v); return true; };
  // the selected points (the grid's selection, by row key)
  P.picked = function(){ var d = this.d, out = []; if (!d) return out; for (var i = 0; i < d.id.length; i++) if (this.sel.has(d.id[i]) && d.x[i] != null && d.y[i] != null) out.push(i); return out; };
  P.zoomSel = function(){ var k = this.picked(), d = this.d; if (!k.length) return; var x0 = Infinity, x1 = -Infinity, y0 = Infinity, y1 = -Infinity;
    k.forEach(function(i){ x0 = Math.min(x0, d.x[i]); x1 = Math.max(x1, d.x[i]); y0 = Math.min(y0, d.y[i]); y1 = Math.max(y1, d.y[i]); });
    var px = Math.max((x1 - x0) * 0.15, (this.full[1] - this.full[0]) * 0.02), py = Math.max((y1 - y0) * 0.15, (this.full[3] - this.full[2]) * 0.02); this.mark(); this.set([x0 - px, x1 + px, y0 - py, y1 + py]); };
  // the bar above the selection: how much is selected, Zoom to this, and the page's own actions (copies of its buttons)
  P.drawAct = function(){ var b = this.act, k = this.picked(); if (!k.length || this.drag || !this.view) { b.hidden = true; return; }
    var acts = document.querySelectorAll('.wb-cellbar .wb-acts button:not([disabled])'), h = '<span class="n">' + k.length + (k.length === 1 ? ' point' : ' points') + '</span><button type="button" data-wb-xact="zoom">Zoom to this</button>';
    acts.forEach(function(a){ if (a.offsetParent !== null && a.id) h += '<button type="button" data-wb-xact="' + esc(a.id) + '"' + (a.classList.contains('danger') ? ' class="danger"' : '') + '>' + esc(a.textContent.replace(/\s+[A-Z]$/, '').trim()) + '</button>'; });
    b.innerHTML = h + '<button type="button" data-wb-xact="clear" title="Esc">Clear</button>'; b.hidden = false;
    var d = this.d, sx = 0; k.forEach(function(i){ sx += d.x[i]; }); var x = this.X(sx / k.length), W = this.el.clientWidth, bw = b.offsetWidth; b.style.left = Math.max(0, Math.min(W - bw, x - bw / 2)) + 'px'; };
  P.colour = function(c){ var d = this.d, k = this.cols; if (c == null || !isFinite(c) || !k.length) return null;
    var lo = d.clim[0], hi = d.clim[1], f = hi > lo ? (c - lo) / (hi - lo) : 0.5; f = Math.max(0, Math.min(1, f));
    if (k.length === 1) return k[0]; var p = f * (k.length - 1), i = Math.min(k.length - 2, Math.floor(p)), t = p - i, a = k[i], b = k[i + 1];
    return [Math.round(a[0] + (b[0] - a[0]) * t), Math.round(a[1] + (b[1] - a[1]) * t), Math.round(a[2] + (b[2] - a[2]) * t)]; };
  P.radius = function(){ var n = 0, d = this.d, v = this.view;
    for (var i = 0; i < d.x.length; i++) if (d.x[i] >= v[0] && d.x[i] <= v[1] && d.y[i] >= v[2] && d.y[i] <= v[3]) n++;
    return Math.max(2, Math.min(7, 0.42 * Math.sqrt(this.pw() * this.ph() / Math.max(n, 1)))); };
  P.draw = function(){ var c = this.ctx, W = this.W, H = this.H; if (!W || !H) return;
    var C = {bg: cssv('--wb-bg', '#1f1f28'), fg: cssv('--wb-fg', '#dcd7ba'), fg2: cssv('--wb-fg2', '#c8c093'), fg3: cssv('--wb-fg3', '#727169'), grid: cssv('--wb-sel', '#2a2a37'),
             warn: cssv('--wb-warn', '#e6c384'), err: cssv('--wb-err', '#e46876'), ok: cssv('--wb-ok', '#98bb6c'), hl: cssv('--wb-acc2', '#7fb4ca'), mono: cssv('--wb-mono', 'monospace')};
    c.clearRect(0, 0, W, H); var d = this.d;
    if (!d || !d.x.length || !this.view) { c.fillStyle = C.fg3; c.font = '12px ' + C.mono; c.textBaseline = 'top'; c.fillText(d ? 'No points to draw.' : '', this.L, 12); this.tipShow(); return; }
    var self = this, L = this.L, T = this.T, pw = this.pw(), ph = this.ph(), v = this.view;
    var xt = nice(v[0], v[1], Math.max(2, Math.floor(pw / 90))), yt = nice(v[2], v[3], Math.max(2, Math.floor(ph / 40)));
    c.strokeStyle = C.grid; c.lineWidth = 1;
    xt.ticks.forEach(function(x){ var px = Math.round(self.X(x)) + 0.5; if (px < L || px > L + pw) return; c.beginPath(); c.moveTo(px, T); c.lineTo(px, T + ph); c.stroke(); });
    yt.ticks.forEach(function(y){ var py = Math.round(self.Y(y)) + 0.5; if (py < T || py > T + ph) return; c.beginPath(); c.moveTo(L, py); c.lineTo(L + pw, py); c.stroke(); });
    c.save(); c.beginPath(); c.rect(L, T, pw, ph); c.clip();
    var r = this.radius(), n = d.x.length, order = [], i;
    for (i = 0; i < n; i++) order.push(i);
    var rank = function(k){ var s = d.s[k] || ''; return self.sel.has(d.id[k]) ? 3 : s ? 2 : 1; };
    order.sort(function(a, b){ return rank(a) - rank(b); });
    order.forEach(function(k){ var x = d.x[k], y = d.y[k]; if (x == null || y == null) return; var px = self.X(x), py = self.Y(y);
      if (px < L - 8 || px > L + pw + 8 || py < T - 8 || py > T + ph + 8) return;
      var col = self.colour(d.c[k]), s = d.s[k] || '', on = self.sel.has(d.id[k]);
      if (col) { c.fillStyle = 'rgb(' + col.join(',') + ')'; c.beginPath(); c.arc(px, py, r, 0, 6.2832); c.fill(); }
      else { c.strokeStyle = C.fg3; c.lineWidth = 1.2; c.beginPath(); c.moveTo(px - r, py - r); c.lineTo(px + r, py + r); c.moveTo(px + r, py - r); c.lineTo(px - r, py + r); c.stroke(); }
      if (s) { c.strokeStyle = s === 'flag' ? C.warn : s === 'pend' ? C.err : C.ok; c.lineWidth = 1.6; if (s !== 'flag') c.setLineDash([2, 2]);
        c.beginPath(); c.arc(px, py, r + 1.8, 0, 6.2832); c.stroke(); c.setLineDash([]); }
      if (on) { c.strokeStyle = C.hl; c.lineWidth = 2.2; c.beginPath(); c.arc(px, py, r + (s ? 4 : 2.6), 0, 6.2832); c.stroke(); } });
    if (this.active != null) { var a = d.id.indexOf(this.active); if (a >= 0 && d.x[a] != null) { var ax = Math.round(this.X(d.x[a])) + 0.5, ay = Math.round(this.Y(d.y[a])) + 0.5;
      c.strokeStyle = C.hl; c.lineWidth = 1; c.setLineDash([3, 3]); c.beginPath(); c.moveTo(ax, T); c.lineTo(ax, T + ph); c.moveTo(L, ay); c.lineTo(L + pw, ay); c.stroke(); c.setLineDash([]); } }
    c.restore();
    c.strokeStyle = C.grid; c.lineWidth = 1; c.strokeRect(L + 0.5, T + 0.5, pw - 1, ph - 1);
    c.fillStyle = C.fg2; c.font = '10.5px ' + C.mono; c.textAlign = 'center'; c.textBaseline = 'top';
    xt.ticks.forEach(function(x){ var px = self.X(x); if (px >= L && px <= L + pw) c.fillText(x.toFixed(xt.digits).replace('-', '\u2212'), px, T + ph + 5); });
    c.textAlign = 'right'; c.textBaseline = 'middle';
    yt.ticks.forEach(function(y){ var py = self.Y(y); if (py >= T + 4 && py <= T + ph - 4) c.fillText(y.toFixed(yt.digits).replace('-', '\u2212'), L - 6, py); });
    c.fillStyle = C.fg; c.font = '11px ' + C.mono; c.textAlign = 'center'; c.textBaseline = 'bottom'; c.fillText(d.xlab || '', L + pw / 2, H - 2);
    c.save(); c.translate(13, T + ph / 2); c.rotate(-Math.PI / 2); c.textBaseline = 'middle'; c.fillText(d.ylab || '', 0, 0); c.restore();
    if (this.cols.length) { var bx = L + pw + 16, bw = 11, g = c.createLinearGradient(0, T + ph, 0, T);
      this.cols.forEach(function(k, j){ g.addColorStop(self.cols.length > 1 ? j / (self.cols.length - 1) : 0, 'rgb(' + k.join(',') + ')'); });
      c.fillStyle = g; c.fillRect(bx, T, bw, ph); c.strokeStyle = C.grid; c.strokeRect(bx + 0.5, T + 0.5, bw - 1, ph - 1);
      c.fillStyle = C.fg2; c.font = '10px ' + C.mono; c.textAlign = 'left'; c.textBaseline = 'middle'; var lo = d.clim[0], hi = d.clim[1];
      d.cticks.forEach(function(t, j){ if (!(hi > lo) || t < lo - 1e-9 || t > hi + 1e-9) return; var py = T + ph - (t - lo) / (hi - lo) * ph; c.fillText(String(d.clabs[j] != null ? d.clabs[j] : t), bx + bw + 4, py); });
      c.save(); c.translate(W - 6, T + ph / 2); c.rotate(-Math.PI / 2); c.textAlign = 'center'; c.textBaseline = 'bottom'; c.fillStyle = C.fg; c.font = '11px ' + C.mono; c.fillText(d.clab || '', 0, 0); c.restore(); }
    if (this.drag && this.drag.mode !== 'pan' && (Math.abs(this.drag.x - this.drag.x0) > 3 || Math.abs(this.drag.y - this.drag.y0) > 3)) {
      var x0 = Math.max(L, Math.min(this.drag.x0, this.drag.x)), x1 = Math.min(L + pw, Math.max(this.drag.x0, this.drag.x)),
          y0 = Math.max(T, Math.min(this.drag.y0, this.drag.y)), y1 = Math.min(T + ph, Math.max(this.drag.y0, this.drag.y)), selm = this.drag.mode === 'select';
      c.globalAlpha = selm ? 0.22 : 0.1; c.fillStyle = selm ? C.hl : C.fg; c.fillRect(x0, y0, x1 - x0, y1 - y0); c.globalAlpha = 1;
      c.strokeStyle = C.hl; c.lineWidth = 1; c.strokeRect(x0 + 0.5, y0 + 0.5, Math.max(1, x1 - x0 - 1), Math.max(1, y1 - y0 - 1)); }
    this.tipShow(); };
  P.nearest = function(px, py, maxd){ var d = this.d, best = -1, bd = maxd * maxd; if (!d) return -1;
    for (var i = 0; i < d.x.length; i++) { if (d.x[i] == null || d.y[i] == null) continue; var dx = this.X(d.x[i]) - px, dy = this.Y(d.y[i]) - py, q = dx * dx + dy * dy; if (q < bd) { bd = q; best = i; } }
    return best; };
  P.tipShow = function(){ var tip = this.tip, d = this.d, k = this.hover; if (!tip) return; if (k < 0 || this.drag || !d) { tip.style.display = 'none'; return; }
    var rows = ['<div class="h">' + esc((d.idlab || '#') + d.id[k]) + (d.lab.length ? ' \u00b7 ' + esc(d.lab[k]) : '') + '</div>',
                '<div><span>' + esc(d.vlab || d.clab || 'value') + '</span><b>' + vtxt(d.v.length ? d.v[k] : d.c[k]) + '</b></div>',
                '<div><span>' + esc(d.xlab || 'x') + '</span><b>' + vtxt(d.x[k]) + '</b></div>', '<div><span>' + esc(d.ylab || 'y') + '</span><b>' + vtxt(d.y[k]) + '</b></div>'];
    if (d.s[k]) rows.push('<div><span>state</span><b>' + esc(STATE[d.s[k]] || d.s[k]) + '</b></div>');
    tip.innerHTML = rows.join(''); tip.style.display = 'block'; var hx = this.X(d.x[k]), hy = this.Y(d.y[k]), x = hx + 14, w = tip.offsetWidth;
    if (x + w > this.W - 4) x = hx - w - 14; tip.style.left = Math.max(4, x) + 'px'; tip.style.top = Math.max(4, Math.min(hy - 10, this.H - tip.offsetHeight - 4)) + 'px'; };
  P.pos = function(e){ var r = this.cv.getBoundingClientRect(); return {x: e.clientX - r.left, y: e.clientY - r.top}; };
  P.inside = function(q){ return q.x >= this.L && q.x <= this.L + this.pw() && q.y >= this.T && q.y <= this.T + this.ph(); };
  P.down = function(e){ if ((e.button !== 0 && e.button !== 2) || !this.d || !this.view) return; var q = this.pos(e); if (!this.inside(q)) return; e.preventDefault();
    var right = e.button === 2 || (e.ctrlKey && /Mac/.test(navigator.platform)), m = !right && (SPACE || e.altKey) ? 'pan' : 'select';
    if (m === 'pan') this.mark(); this.act.hidden = true;
    this.drag = {x0: q.x, y0: q.y, x: q.x, y: q.y, mode: m, view: this.view.slice(), add: e.shiftKey || e.metaKey || (e.ctrlKey && !right), right: right}; this.cv.style.cursor = m === 'pan' ? 'grabbing' : 'crosshair'; };
  P.move = function(e){ var q = this.pos(e);
    if (this.drag) { this.drag.x = q.x; this.drag.y = q.y;
      if (this.drag.mode === 'pan') { var v = this.drag.view, dx = (this.drag.x0 - q.x) / this.pw() * (v[1] - v[0]), dy = (q.y - this.drag.y0) / this.ph() * (v[3] - v[2]);
        this.view = [v[0] + dx, v[1] + dx, v[2] + dy, v[3] + dy]; }
      this.draw(); return; }
    if (e.target === this.cv) this.cv.style.cursor = SPACE ? 'grab' : 'crosshair';
    var k = e.target === this.cv && this.inside(q) ? this.nearest(q.x, q.y, 10) : -1;
    if (k !== this.hover) { this.hover = k; this.draw(); } };
  P.up = function(e){ var d = this.drag; if (!d) return; this.drag = null; this.cv.style.cursor = '';
    var still = Math.abs(d.x - d.x0) <= 3 && Math.abs(d.y - d.y0) <= 3;
    if (d.mode === 'pan') { if (still) this.hist.pop(); this.draw(); this.drawAct(); return; }
    if (still) { if (d.right) { this.drawAct(); return; } var k = this.nearest(d.x0, d.y0, 12);
      if (window.wbGridSelectRows) window.wbGridSelectRows(k >= 0 ? [this.d.id[k]] : [], d.add); this.draw(); return; }
    var a = [this.ix(Math.min(d.x0, d.x)), this.ix(Math.max(d.x0, d.x)), this.iy(Math.max(d.y0, d.y)), this.iy(Math.min(d.y0, d.y))];
    var ids = [], m = this.d; for (var i = 0; i < m.x.length; i++) if (m.x[i] >= a[0] && m.x[i] <= a[1] && m.y[i] >= a[2] && m.y[i] <= a[3]) ids.push(m.id[i]);
    if (window.wbGridSelectRows) window.wbGridSelectRows(ids, d.add); this.draw(); };
  P.wheel = function(e){ if (!this.d || !this.view) return; var q = this.pos(e); if (!this.inside(q)) return; e.preventDefault();
    var dy = e.deltaMode === 1 ? e.deltaY * 16 : e.deltaY; this.zoomBy(Math.min(1.2, Math.max(1 / 1.2, Math.exp(dy * 0.0015))), [this.ix(q.x), this.iy(q.y)]); };

  var xy = null, SPACE = false;
  function get(){ if (!xy) { var el = document.getElementById('wb_xy'); if (el) xy = window.wbXY = new XY(el); } return xy; }
  $(function(){ get(); if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler('wb-xy', function(m){ var c = get(); if (c) c.setData(m); }); });
  $(document).on('shown.bs.tab', function(){ if (xy) setTimeout(function(){ xy.resize(); }, 30); });
  $(document).on('shiny:inputchanged', function(e){ if (e.name === 'wb_layout' && xy) { xy.el.classList.toggle('tall', e.value === 'chart'); setTimeout(function(){ xy.resize(); }, 30); } });
  $(document).on('click', '[data-wb-xy]', function(e){ e.preventDefault(); var c = get(); if (!c || !c.view) return; var a = this.getAttribute('data-wb-xy');
    if (a !== 'back') c.mark(); if (a === 'in') c.zoomBy(0.5); else if (a === 'out') c.zoomBy(2); else if (a === 'reset') c.reset(); else if (a === 'back') c.goBack(); });
  $(document).on('click', '[data-wb-xact]', function(e){ e.preventDefault(); var c = get(), a = this.getAttribute('data-wb-xact'); if (!c) return;
    if (a === 'zoom') c.zoomSel(); else if (a === 'clear') { if (window.wbGridClear) window.wbGridClear(); } else { var b = document.getElementById(a); if (b) b.click(); } });
  // Space held down makes a drag move the view; Backspace over the chart goes back a view; + and - zoom
  $(document).on('keydown', function(e){ if ($(e.target).is('input,textarea,select,[contenteditable]') || !xy || !xy.el.matches(':hover')) return;
    if (e.key === ' ') { SPACE = true; e.preventDefault(); } else if (e.key === 'Backspace' && xy.goBack()) e.preventDefault();
    else if (!e.metaKey && !e.ctrlKey && !e.altKey && (e.key === '+' || e.key === '=' || e.key === '-' || e.key === '_')) { xy.mark(); xy.zoomBy(e.key === '-' || e.key === '_' ? 1.5 : 1 / 1.5); e.preventDefault(); } });   // + and - zoom (standards A11)
  $(document).on('keyup', function(e){ if (e.key === ' ') SPACE = false; });
})();

