const express = require('express');

exports._getHeaders = function (req) {
  return req.headers;
};

exports._getHostname = function (req) {
  return req.hostname;
};

exports._getMethod = function (req) {
  const method = req.method && typeof req.method === 'string'
    ? req.method.toLowerCase()
    : "";
  return method;
};

exports._getOriginalUrl = function (req) {
  return req.originalUrl;
};

exports._getQuery = function (req) {
  return req.query;
};

exports._makeAccepts = function (just) {
  return function (nothing) {
    return function (req) {
      return function (types) {
        const t = req.accepts(types);
        return t && typeof t === "string" ? just(t) : nothing;
      };
    };
  };
};

exports._makeRouter = function () {
  return express.Router();
};

exports._makeJsonMiddleware = function (type) {
  return express.json({ type: type });
};

exports._useRouter = function (path, router, app) {
  app.use(path, router);
};

exports._useRouterMiddleware = function (path, middleware, router) {
  router.use(path, middleware);
};

exports._useSubRouter = function (path, parent, child) {
  parent.use(path, child);
};

exports._delete = function (path, handler, router) {
  router.delete(path, handler);
}

exports._get = function (path, handler, router) {
  router.get(path, handler);
}

exports._post = function (path, handler, router) {
  router.post(path, handler);
}

exports._put = function (path, handler, router) {
  router.put(path, handler);
}
