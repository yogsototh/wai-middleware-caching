# Caching Wai Middlewares

This repository provide WAI middlewares caching ability.

To minimize code dependency, there are three different packages.

- `wai-middleware-caching`: cache backend agnostic cache. You'll need to provide a `CacheBackend` value to make it works.
- `wai-middleware-caching-lru`: Use `lrucache` as backend (RAM only)
- `wai-middleware-caching-redis`: Use Redis as backend to cache requests.
