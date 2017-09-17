

exports.fetchImpl = function (url) {
  return function() {
    return fetch(url);
  };
}

exports.trends = function() {
      console.log("test");
var Trending = require("github-trend");
var scraper = new Trending.Scraper();
    return scraper.scrapeTrendingRepos("").then(function(repos){
      return repos;
    repos.forEach(function(repo){
      // return repo.name
        console.log(repo.owner);
      console.log(repo.name);
    });
}).catch(function(err){
    console.log(err.message);
      return err.message
});
};


