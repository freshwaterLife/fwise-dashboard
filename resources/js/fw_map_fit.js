// fw_map_fit.js
// Sets each map's zoom-out limit to the fractional zoom at which one world
// fills its width (on load and resize), so the whole band shows and the world
// never repeats (tiles don't wrap; see fw_leaflet()).
//
// Leaflet snaps zooms to whole steps before clamping to minZoom, so
// _limitZoom returns the limit itself at or below it; other zooms still snap.
//
// A Leaflet init hook, not onRender, so the card script stays the only render
// hook and saved reports work without Shiny.
(function () {
  if (!window.L || !L.Map || L.Map.fwFitWidth) return;
  L.Map.fwFitWidth = true;

  // Rebuild cluster groups when minZoom drops: markercluster builds its tree
  // once from the old minZoom and would show every marker as one ring. The
  // card script's wiring (fwWired) isn't doubled by the re-add.
  function reclusterBelow(map) {
    if (!L.MarkerClusterGroup) return;
    var floor = Math.floor(map.getMinZoom());
    map.eachLayer(function (layer) {
      if (!(layer instanceof L.MarkerClusterGroup) || !layer._topClusterLevel) return;
      if (layer._topClusterLevel._zoom < floor) return;
      var markers = layer.getLayers();
      layer.clearLayers();
      layer.addLayers(markers);
    });
  }

  // Gotcha: markercluster's _recursively walks start from the raw fractional
  // minZoom, so ungrouped markers vanish. Floor the start. Patched lazily in
  // addLayer, since on a Shiny page the plugin loads after the map exists.
  function patchCluster() {
    if (!L.MarkerCluster || L.MarkerCluster.fwFloored) return;
    L.MarkerCluster.fwFloored = true;
    var walk = L.MarkerCluster.prototype._recursively;
    L.MarkerCluster.prototype._recursively = function (bounds, from, to, a, b) {
      return walk.call(this, bounds, Math.floor(from), to, a, b);
    };
  }

  var addLayer = L.Map.prototype.addLayer;
  L.Map.prototype.addLayer = function (layer) {
    patchCluster();
    return addLayer.call(this, layer);
  };

  L.Map.addInitHook(function () {
    var map = this;
    var floor = map.options.minZoom || 0;
    var limitZoom = map._limitZoom;
    map._limitZoom = function (zoom) {
      var min = this.getMinZoom();
      return zoom <= min + 1e-6 ? min : limitZoom.call(this, zoom);
    };
    function fit() {
      var w = map.getSize().x;
      // A hidden map has no width; it fits on the resize when shown.
      if (!w) return;
      var z = Math.max(floor, Math.log(w / 256) / Math.LN2);
      var was = map.getMinZoom();
      // A map at its limit follows the limit; a zoomed-in map stays put.
      var follow = map.getZoom() <= was + 1e-6 || map.getZoom() < z;
      if (was !== z) {
        map.setMinZoom(z);
        reclusterBelow(map);
      }
      if (follow) map.setZoom(z, { animate: false });
    }
    map.whenReady(fit);
    map.on('resize', fit);
  });
})();
