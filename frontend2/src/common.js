export const compose = (...fs) => y => Array.prototype.reduceRight.call(fs, (x, f) => f(x), y);
 
export const getJSON = (url, cb) => fetch(url).then(r => r.json().then(cb));

