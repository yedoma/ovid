# R/export.R  --  The lightweight archive viewer: one folder (and a zip of it) that someone without the archive can open.
# Part of Ovid Core // RS. Sourced by rs_pipeline.R.
#
# index.html holds everything it shows (flights, their links, a thinned set of image positions) as JSON inside the page,
# so it opens by double-click with no server and no R; only the map tiles and the Leaflet library come from the internet.
# The CSV files beside it are the same tables for people who want to work with them. Nothing in the archive is copied.


# Image positions thinned to at most `per_flight` per flight, enough to draw each flight's pattern on the map.
thin_points <- function(images, per_flight = 150L) {
  p <- images[primary == TRUE & is.finite(lat) & is.finite(lon) & !(lat == 0 & lon == 0), .(flight_id, lat = round(lat, 6), lon = round(lon, 6))]
  if (!nrow(p)) return(p)                                                            # no image has a position
  p[, i := seq_len(.N), by = flight_id][, n := .N, by = flight_id]
  p[i %in% unique(round(seq(1, n[1], length.out = min(n[1], per_flight)))), .(flight_id, lat, lon), by = n][, n := NULL][]
}

export_viewer <- function(out, links, images, scanned, geo, dir, title = "ovid RS archive", datasets = NULL, gis = NULL) {
  unlink(dir, TRUE); dir.create(dir, recursive = TRUE)
  fl <- out[is.na(exclude_from_archive) | !tolower(exclude_from_archive) %in% c("true", "yes", "1", "x")]
  keep <- c("flight_id", "display_name", "year", "date", "start", "duration_min", "project", "site", "family", "aircraft", "sensor", "flight_no",
            "n_images", "n_georef", "size_gb", "positioning_status", "positioning_evidence", "has_log", "has_geotags", "has_mrk", "has_rpt",
            "has_gnss_obs", "has_lidar_raw", "has_mission", "source_id", "folder", "copies", "qa_flags", "notes", "lat_min", "lat_max", "lon_min", "lon_max")
  fl <- fl[, intersect(keep, names(fl)), with = FALSE]
  lk <- links[flight_id %in% fl$flight_id & state != "suggested", .(flight_id, file, role, state, evidence)]
  pts <- thin_points(images[flight_id %in% fl$flight_id])
  fwrite(fl, file.path(dir, "flights.csv")); fwrite(lk, file.path(dir, "links.csv")); fwrite(scanned, file.path(dir, "sources.csv")); fwrite(geo, file.path(dir, "geometry.csv"))
  # satellite, aerial and LiDAR footprints and GIS features: one copy each, colours as in the app
  dcol <- c(satellite = PAL$blue, aerial = PAL$bone, lidar = PAL$green, raster = PAL$gray)
  gcol <- c(`GCPs and checkpoints` = PAL$gold, `Plots, transects and grids` = PAL$teal, `Boundaries and extents` = PAL$bone, `Field sites and trails` = PAL$green, `Other GIS layers` = PAL$gray)
  ds <- if (!is.null(datasets) && nrow(datasets)) datasets[!is.na(footprint) & !duplicated(paste(folder, footprint))] else data.table()
  if (nrow(ds)) { fwrite(datasets, file.path(dir, "datasets.csv"))
    ds <- ds[, .(group = kind, line = footprint, source = grepl("vendor", geometry_from), col = unname(dcol[kind]), type = "polygon",
                 label = paste0(platform, " \u00b7 ", product, " \u00b7 ", fifelse(is.na(acquired), "date unknown", acquired), " \u00b7 ", file))] }
  gs <- if (!is.null(gis) && nrow(gis)) gis[!duplicated(paste(category, type, line))] else data.table()
  if (nrow(gs)) { fwrite(gs, file.path(dir, "gis_features.csv"))
    gs <- gs[, .(group = category, line, source = TRUE, col = unname(gcol[category]), type, label = paste0(category, " \u00b7 ", fifelse(is.na(feature), "", feature), " \u00b7 ", rel))] }
  extra <- rbind(ds, gs, fill = TRUE)
  data <- jsonlite::toJSON(list(title = title, extra = extra, made = format(Sys.time(), "%Y-%m-%d %H:%M"), app = paste(APP_NAME, APP_VERSION),
                                sources = scanned, flights = fl, links = lk, points = pts, colours = as.list(STATUS_COL), geometry = geo[is.na(flight_id) | flight_id %in% fl$flight_id & kind != "derived"], states = POSITIONING),
                           dataframe = "rows", na = "null", auto_unbox = TRUE, digits = NA)
  page <- sub("__DATA__", gsub("</", "<\\\\/", data), VIEWER_HTML, fixed = TRUE)
  tools <- Filter(file.exists, file.path(c(get0("APP_DIR", ifnotfound = "."), get0("PIPELINE_DIR", ifnotfound = ".")), "www", "map_tools.js"))   # Legend on/off, 250 ppi PNG
  page <- sub("__MAPTOOLS__", if (length(tools)) paste(readLines(tools[1], warn = FALSE, encoding = "UTF-8"), collapse = "\n") else "", page, fixed = TRUE)
  writeLines(sub("__TITLE__", html_esc(title), page, fixed = TRUE), file.path(dir, "index.html"), useBytes = TRUE)
  zip_dir(dir, paste0(dir, ".zip"))
  invisible(paste0(dir, ".zip"))
}

VIEWER_HTML <- r"---(<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>__TITLE__</title>
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.9.4/leaflet.min.css">
<script src="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.9.4/leaflet.min.js"></script>
<style>
:root{--bg:#282c34;--panel:#383c44;--fg:#bcbfc4;--muted:#999999;--line:#484c54;--accent:#e5c07b}   /* accent.vim, as the app */
*{box-sizing:border-box}body{margin:0;font:14px/1.4 system-ui,sans-serif;background:var(--bg);color:var(--fg)}
header{padding:12px 16px;border-bottom:1px solid var(--line);display:flex;flex-wrap:wrap;gap:12px;align-items:baseline}
h1{font-size:17px;margin:0}.muted{color:var(--muted);font-size:12px}
#bar{display:flex;flex-wrap:wrap;gap:8px;padding:8px 16px}select,input{font:inherit;padding:4px 6px;background:var(--panel);color:var(--fg);border:1px solid var(--line);border-radius:6px}
#cards{display:flex;flex-wrap:wrap;gap:8px;padding:0 16px 8px}.card{background:var(--panel);border:1px solid var(--line);border-radius:8px;padding:6px 10px}.card b{display:block;font-size:16px}
main{display:grid;grid-template-columns:1fr 1fr;gap:8px;padding:0 16px 16px;height:calc(100vh - 190px);min-height:420px}
@media (max-width:900px){main{grid-template-columns:1fr;height:auto}#map{height:360px}#tbl{max-height:none}}
#map{border:1px solid var(--line);border-radius:8px}#tbl{overflow:auto;background:var(--panel);border:1px solid var(--line);border-radius:8px}
table{border-collapse:collapse;width:100%;font-size:12.5px}th,td{padding:4px 6px;border-bottom:1px solid var(--line);text-align:left;white-space:nowrap}
th{position:sticky;top:0;background:var(--panel);cursor:pointer}tr.sel{background:color-mix(in srgb,var(--accent) 18%,transparent)}tbody tr{cursor:pointer}
.dot{display:inline-block;width:9px;height:9px;border-radius:50%;margin-right:4px}
.map-legend{background:var(--panel);color:var(--fg);padding:6px 9px;border-radius:6px;font-size:12px;line-height:1.5;max-height:40vh;overflow:auto;border:1px solid var(--line)}
.map-legend i{display:inline-block;width:14px;height:0;border-top:3px solid;margin-right:6px;vertical-align:middle}.map-legend .lg-t{font-weight:600;margin-top:3px}.lg-n{color:var(--muted)}
#detail{padding:8px 10px;border-top:1px solid var(--line);font-size:12.5px;white-space:normal}
</style></head><body>
<header><h1 id="t"></h1><span class="muted" id="sub"></span></header>
<div id="bar"><select id="fy"><option value="">All years</option></select><select id="fs"><option value="">All sites</option></select>
<select id="fa"><option value="">All sensors</option></select><select id="fp"><option value="">All positioning</option></select><input id="q" placeholder="Search"></div>
<div id="cards"></div><div class="muted" style="padding:0 16px 6px">Map: dashed = planned &middot; solid = observed (flown track, image positions, receivers) &middot; dotted = derived by Ovid</div>
<main><div id="map"></div><div id="tbl"><table><thead><tr id="hd"></tr></thead><tbody id="bd"></tbody></table><div id="detail" class="muted">Select a flight for its files and evidence.</div></div></main>
<script id="data" type="application/json">__DATA__</script>
<script>__MAPTOOLS__</script>
<script>
const D=JSON.parse(document.getElementById('data').textContent),F=D.flights;
const COL=D.colours;
document.getElementById('t').textContent=D.title;
document.getElementById('sub').textContent=`${F.length} flights from ${D.sources.length} source(s) \u00b7 made ${D.made} with ${D.app}`;
const uniq=k=>[...new Set(F.map(f=>f[k]).filter(v=>v!==null&&v!==''))].sort();
[['fy','year'],['fs','site'],['fa','sensor'],['fp','positioning_status']].forEach(([id,k])=>uniq(k).forEach(v=>{const o=document.createElement('option');o.textContent=v;document.getElementById(id).append(o)}));
const cols=[['date','Date'],['site','Site'],['aircraft','Aircraft'],['sensor','Sensor'],['n_images','Images'],['size_gb','GB'],['positioning_status','Positioning'],['qa_flags','QA']];
let sortK='date',asc=true,sel=null;
document.getElementById('hd').innerHTML=cols.map(c=>`<th data-k="${c[0]}">${c[1]}</th>`).join('');
document.querySelectorAll('th').forEach(th=>th.onclick=()=>{asc=sortK===th.dataset.k?!asc:true;sortK=th.dataset.k;draw()});
const map=L.map('map');L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',{maxZoom:19,attribution:'Esri World Imagery'}).addTo(map);
const lay=L.layerGroup().addTo(map),P={},G={},X={};
// satellite / aerial / LiDAR footprints (solid: vendor corners, dashed: from headers) and GIS layers, each kind switchable
(D.extra||[]).forEach(x=>{const ll=x.line.split(';').map(s=>s.split(' ').map(Number));const g=X[x.group]=X[x.group]||L.layerGroup().addTo(map);
 const m=x.type==='point'?L.circleMarker(ll[0],{radius:4,color:'#282c34',weight:1,fillColor:x.col,fillOpacity:1}):x.type==='line'?L.polyline(ll,{color:x.col,weight:2}):
  L.polygon(ll,{color:x.col,weight:2,fillOpacity:.1,dashArray:x.source?null:'6 4'});m.bindTooltip(x.label.replace(/[&<>]/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;'}[c]))).addTo(g)});
if(Object.keys(X).length)L.control.layers(null,X,{collapsed:true}).addTo(map);
// legend: positioning colours, then each switchable layer that is on; Legend button hides it, PNG saves the view at 250 ppi
const XC={};(D.extra||[]).forEach(x=>XC[x.group]=XC[x.group]||x.col);const leg=L.control({position:'bottomright'});leg.onAdd=()=>L.DomUtil.create('div','map-legend');leg.addTo(map);
function legend(){const e=s=>String(s).replace(/[&<>]/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;'}[c])),st=[...new Set(F.map(f=>f.positioning_status))].filter(k=>COL[k]),on=Object.keys(X).filter(g=>map.hasLayer(X[g]));
 leg.getContainer().innerHTML='<div class="lg-t">Flights by positioning</div>'+st.map(k=>`<div><i style="border-color:${COL[k]}"></i>${e(k)}</div>`).join('')+(on.length?'<div class="lg-t">Layers</div>'+on.map(g=>`<div><i style="border-color:${XC[g]}"></i>${e(g)}</div>`).join(''):'')+'<div class="lg-n">dashed planned \u00b7 solid observed \u00b7 dotted derived</div>'}
legend();map.on('overlayadd overlayremove',legend);if(window.OvidMapTools)OvidMapTools(map,{legend:()=>leg.getContainer(),name:'viewer_map',key:'viewer'});D.points.forEach(p=>(P[p.flight_id]=P[p.flight_id]||[]).push([p.lat,p.lon]));
(D.geometry||[]).forEach(g=>(G[g.flight_id||'']=G[g.flight_id||'']||[]).push(g));
// planned: dashed line; observed: solid line or marker; derived: dotted outline
function shape(g,c){const ll=g.line.split(';').map(s=>s.split(' ').map(Number));const tip=`${g.kind}: ${g.what}${g.ref?' \u00b7 '+g.ref:''}`;
 if(ll.length===1)L.circleMarker(ll[0],{radius:6,color:c,weight:2,fill:false}).bindTooltip(tip).addTo(lay);
 else L.polyline(ll,{color:c,weight:g.kind==='planned'?1.5:2.2,dashArray:g.kind==='planned'?'8 6':null,opacity:.9}).bindTooltip(tip).addTo(lay)}
function shown(){const y=fy.value,s=fs.value,a=fa.value,p=fp.value,q=document.getElementById('q').value.toLowerCase();
 return F.filter(f=>(!y||String(f.year)===y)&&(!s||f.site===s)&&(!a||f.sensor===a)&&(!p||f.positioning_status===p)&&(!q||JSON.stringify(f).toLowerCase().includes(q)))}
function draw(){const r=shown().sort((a,b)=>{const x=a[sortK],y=b[sortK];return (x>y?1:x<y?-1:0)*(asc?1:-1)});
 const n=r.reduce((s,f)=>s+(f.n_images||0),0),gb=r.reduce((s,f)=>s+(f.size_gb||0),0),c={};r.forEach(f=>c[f.positioning_status]=(c[f.positioning_status]||0)+1);
 document.getElementById('cards').innerHTML=`<div class="card"><b>${r.length}</b>flights</div><div class="card"><b>${n.toLocaleString()}</b>images</div><div class="card"><b>${gb.toFixed(1)}</b>GB</div>`+
  Object.entries(c).map(([k,v])=>`<div class="card"><b>${v}</b><span class="dot" style="background:${COL[k]}"></span>${k}</div>`).join('');
 document.getElementById('bd').innerHTML=r.map(f=>`<tr data-id="${f.flight_id}" class="${f.flight_id===sel?'sel':''}">`+cols.map(([k])=>`<td>${k==='positioning_status'?`<span class="dot" style="background:${COL[f[k]]}"></span>`:''}${f[k]??''}</td>`).join('')+'</tr>').join('');
 document.querySelectorAll('#bd tr').forEach(tr=>tr.onclick=()=>pick(tr.dataset.id,true));
 lay.clearLayers();const b=[];
 r.forEach(f=>{const c=COL[f.positioning_status],pts=P[f.flight_id];
  if(f.lat_min!==null){const bb=[[f.lat_min,f.lon_min],[f.lat_max,f.lon_max]];b.push(...bb);
   L.rectangle(bb,{color:c,weight:f.flight_id===sel?2.5:1,fill:false,dashArray:'1 4'}).bindTooltip(`${f.date} ${f.site||''} \u00b7 ${f.sensor||''} \u00b7 derived extent`).on('click',()=>pick(f.flight_id,false)).addTo(lay)}
  (pts||[]).forEach(p=>L.circleMarker(p,{radius:2,color:c,weight:1,fillOpacity:.9}).bindTooltip('observed image position').addTo(lay));
  (G[f.flight_id]||[]).forEach(g=>shape(g,c))});
 if(!fy.value&&!fs.value&&!fa.value&&!fp.value&&!document.getElementById('q').value)(G['']||[]).forEach(g=>shape(g,'#888'));
 if(b.length&&!draw.done){map.fitBounds(b,{padding:[20,20]});draw.done=1}if(!b.length)map.setView([0,0],2)}
function pick(id,zoom){sel=id;const f=F.find(x=>x.flight_id===id);draw();
 if(zoom&&f.lat_min!==null)map.fitBounds([[f.lat_min,f.lon_min],[f.lat_max,f.lon_max]],{padding:[30,30]});
 const l=D.links.filter(x=>x.flight_id===id);
 document.getElementById('detail').innerHTML=`<b>${f.display_name||f.site||id}</b> \u00b7 ${f.date} \u00b7 ${f.aircraft||''} ${f.sensor||''}<br>${f.positioning_status}: ${f.positioning_evidence||''}<br>Folder: ${f.source_id}:${f.folder}${f.copies?'<br>Copies: '+f.copies:''}${f.qa_flags?'<br>QA: '+f.qa_flags:''}`+
  (l.length?'<br><br>'+l.map(x=>`${x.role}: ${x.file} <span class="muted">(${x.state}; ${x.evidence})</span>`).join('<br>'):'')}
['fy','fs','fa','fp','q'].forEach(i=>document.getElementById(i).oninput=draw);draw();
</script></body></html>)---"
