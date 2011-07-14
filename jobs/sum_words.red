{"language":"javascript","source":"
function(v) {
  var r = {};
  for(var i in v) {
    for(var w in v[i]) {
      if(w in r) r[w] += v[i][w]; 
      else r[w] = v[i][w];
    }
  }
  return [r];
}
"}
