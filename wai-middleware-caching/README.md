# Caching WAI Middleware

This is a base to create caching WAI Middlewares

If you want to provide your own middleware you should provide a `CacheBackend`:

~~~~ {.haskell}

--------------------------------------------------------------------------------
-- | The data structure that should contains everything you need to create
-- a cache backend
data CacheBackend cacheContainer cacheKey cacheVal =
  CacheBackend {
  keyFromReq           :: Request -> ByteString -> IO cacheKey -- ^ Get cacheKey from request and its body
  , toCache            :: Request -> ByteString -> IO Bool -- ^ Function to check whether cache or not
  , addToCache         :: cacheContainer -> cacheKey -> cacheVal -> IO () -- ^ Adding to cache
  , actionOnCache      :: Request -> Response -> IO () -- ^ Action to perform before each caching request
  , actionOnCacheMiss  :: Request -> Response -> IO () -- ^ Action to perfom before each cache miss
  , responseToCacheVal :: Response -> IO cacheVal -- ^ Transform response to cached value
  , cacheValToResponse :: cacheVal -> Response -- ^ Transform cached value to response
  , lookupCache        :: cacheContainer -> cacheKey -> IO (Maybe cacheVal) -- ^ cache lookup
  , cacheContainer     :: cacheContainer -- ^ A cache container
  }
~~~~
