function benchmarkAngular() {
    const runs = 10000;
    const root = angular.element(document.querySelector('[ng-app]'));

    function getElemWatchers(element) {
        let isolateWatchers = getWatchersFromScope(element.data().$isolateScope);
        let scopeWatchers = getWatchersFromScope(element.data().$scope);
        let watchers = scopeWatchers.concat(isolateWatchers);
        angular.forEach(element.children(), function (childElement) {
            watchers = watchers.concat(getElemWatchers(angular.element(childElement)));
        });
        return watchers;
    }
    function getWatchersFromScope(scope) {
        if (scope) {
            return scope.$$watchers || [
            ];
        } else {
            return [];
        }
    }
    const watchers = getElemWatchers(root).length;

    let totalTime = 0;

    root
		.injector()
		.invoke(['$rootScope',function($rootScope) {
			for(let i=0; i<runs; ++i) {
				var a = performance.now();
				$rootScope.$apply();
				totalTime += (performance.now()-a);
			}
		}]);

    console.log('Current watchers: ' + watchers);
    console.log('Average of ' + runs + ' digests: ' + totalTime/runs);
}
