
// Load this file after loading an angular application

// Typical usage:  httpUsers('messagescreen')

// ------------------------------------------------------------

function providerType(p) {
	return p[1];
}

function providerName(p) {
	return p[2][0];
}

function providerDependencies(p) {
	function diArrayToProviders(diArray) {
		if(_.isArray(diArray))
			return diArray.slice(0, -1);
		else
			return [];

	}

	if(providerType(p) === 'component') {
		const ctrl = p[2][1]['controller'];
		if(!ctrl)
			return [];
		else
			return ctrl.$inject;
	}
	else {
		return diArrayToProviders(p[2][1]);
	}
}

// ------------------------------------------------------------


function getProvided(moduleName) {
	function getProvidedShallow(moduleName) {
		const mod = angular.module(moduleName);
		return [
			// TODO: also check run and config blocks

			// ...mod._configBlocks,
			// ...mod._runBlocks,
			...mod._invokeQueue,
		];
	}

	function getProvidedRec(moduleName, cache, modulesSeen) {
		if(modulesSeen[moduleName])
			return;

		modulesSeen[moduleName] = true;

		const providers = getProvidedShallow(moduleName);
		_.forEach(providers, (p) => {
			const name = p[2][0];
			cache[name] = p;
		});

		const childModules = angular.module(moduleName).requires;
		_.forEach(childModules, (m) => {
			getProvidedRec(m, cache, modulesSeen);
		});

		return;
	}


	const providers = {};
	const modules = []
	getProvidedRec(moduleName, providers, modules);

	return {providers, modules: _.keys(modules)};
}


function httpUsers(moduleName) {
	function usesHttp(provider, providerCache) {
		const childDependsOnHttp = _(providerDependencies(provider))
			.flatMap(dep => {
				const out = providerCache[dep];
				if(!out) return [];
				else return [out];
			})
			.some((child) => usesHttp(child, providerCache));
		return _.includes(providerDependencies(provider), '$http') || childDependsOnHttp;
	}

	const { providers } = getProvided(moduleName);
	return _.pickBy(providers, (p) => usesHttp(p, providers));
}
