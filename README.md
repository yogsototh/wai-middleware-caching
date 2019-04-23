> This project has two homes.
> It is ok to work in github, still, for a better decentralized web
> please consider contributing (issues, PR, etc...) throught:
>
> https://gitlab.esy.fun/yogsototh/wai-middleware-caching

---


# Caching Wai Middlewares

This repository provide WAI middlewares caching ability.

To minimize code dependency, there are three different packages.

- [`wai-middleware-caching` <img alt="Hackage" src="https://img.shields.io/hackage/v/wai-middleware-caching.svg"/>](http://hackage.haskell.org/package/wai-middleware-caching): Backend agnostic cache middleware. You'll need to provide a `CacheBackend` value to make it works.
- [`wai-middleware-caching-lru` <img alt="Hackage" src="https://img.shields.io/hackage/v/wai-middleware-caching-lru.svg"/>](http://hackage.haskell.org/package/wai-middleware-caching-lru): Use `lrucache` as cache backend (RAM only)
- [`wai-middleware-caching-redis` <img alt="Hackage" src="https://img.shields.io/hackage/v/wai-middleware-caching-redis.svg"/>](http://hackage.haskell.org/package/wai-middleware-caching-redis): Use Redis as cache backend.
