File uploads in (a) Snap
========================

\begin{quote}
“I was also sad, and thinking,\\
When one day I saw you winking,\\
And I heard you sniffle-snuffle,\\
And I saw your feathers ruffle.
\end{quote}

We recently had to add file uploads to a [Haskell][haskell] web
application built with the [Snap][snap] framework. File uploads in
Snap (that is, [`Snap.Util.FileUploads`][1]) have good documentation,
but we thought it would be useful to write a tutorial about how to
implement a basic file uploader. The idea is to write an application
with an HTML form to upload multiple Haskell files (`.hs` or `.lhs`)
that simply displays the contents of the uploaded files.

This blog post was generated from a literate Haskell file that you can
find in the [turbulent-sniffle][turbulent-sniffle] repository. All the
code was tested with [LTS Haskell 3.18][lts] (and [Stackage Nightly
2015-12-13][nightly]), that is, GHC 7.10.2, [snap-core
0.9.8.0][snap-core], and [snap-server 0.9.5.1][snap-server].

The first thing we'll do is create an `index.html` file and add an
HTML form to upload multiple files:

```html
<form action="/" method="post" enctype="multipart/form-data">
  <input type="file" name="files" multiple>
  <button type="submit">Submit</button>
</form>
```

This is just an HTML form with a file input that allows multiple
files. We could make the input required, but we'll use Haskell to
handle everything.

In order to see exactly what we need for adding file uploads to an
existing Haskell application, let's create a Cabal file. In this case,
we'll call it `turbulent-sniffle.cabal`:

```
name: turbulent-sniffle
version: 0.1.0
build-type: Simple
cabal-version: >= 1.22
```

We can now add the `index.html` file as a data file:

```
data-files: index.html
```

And create an executable component:

```
executable turbulent-sniffle
  main-is: Main.lhs
  other-modules: Paths_turbulent_sniffle
  ghc-options: -Wall
  build-depends:
      base >= 4.8 && < 4.9
    , bytestring
    , directory
    , snap-core >= 0.9 && < 0.10
    , snap-server >= 0.9 && < 0.10
    , transformers
  default-language: Haskell2010
```

If you cloned the [turbulent-sniffle][turbulent-sniffle] repository,
you can now build and run the application:

```
$ stack build --exec turbulent-sniffle
```

Or, to use Stackage Nightly instead of LTS Haskell:

```
$ stack build --resolver nightly-2015-12-13 --exec turbulent-sniffle
```

Go to <http://localhost:8000/> and try to upload a Haskell file such
as `Main.lhs`.

\begin{quote}
To myself I sadly said,\\
‘She's neuralgia in her head!\\
That dear head has nothing on it!\\
Ought she not to wear a bonnet?’
\end{quote}

Next, let's take a look at the Haskell code in `Main.lhs`. In this
case, we only need to export the `main` function, which is probably
not the case for a real application:

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE RecordWildCards #-}
>
> module Main
>  ( main
>  )
>  where

Let's take a look at the modules we'll use:

> -- turbulent-sniffle
> import qualified Paths_turbulent_sniffle as Paths
>
> -- base
> import qualified Control.Applicative as Applicative
> import qualified Data.Either as Either
> import Data.Int (Int64)
> import qualified Data.Maybe as Maybe
>
> -- bytestring
> import Data.ByteString (ByteString)
> import qualified Data.ByteString.Char8 as ByteStringChar8
>
> -- directory
> import qualified System.Directory as Directory
>
> -- snap-core
> import Snap.Core
>   ( Method(GET, POST)
>   , Snap
>   )
> import qualified Snap.Core as SnapCore
> import qualified Snap.Util.FileServe as SnapFileServe
> import Snap.Util.FileUploads
>   ( PartInfo(..)
>   , PartUploadPolicy
>   , PolicyViolationException
>   , UploadPolicy
>   )
> import qualified Snap.Util.FileUploads as SnapFileUploads
>
> -- snap-server
> import qualified Snap.Http.Server as SnapServer
>
> -- transformers
> import qualified Control.Monad.IO.Class as MonadIO

The [snap-server][snap-server] package allows us to quickly serve an
application, as follows:

< main :: IO ()
< main =
<   SnapServer.quickHttpServe
<     (SnapCore.ifTop (SnapCore.writeBS "Behold a turbulent sniffle!"))

This application would simply print a string, which is not what we
want. Instead, let's add a handler for the top (`/`) route:

> main :: IO ()
> main =
>   SnapServer.quickHttpServe (SnapCore.ifTop handleTop)

The `handleTop` handler will take care of a GET request to display the
`index.html` file and a POST request to upload files (that is, to
handle the form submission):

> handleTop :: Snap ()
> handleTop =
>   SnapCore.method GET handleTop'
>     Applicative.<|> SnapCore.method POST handleFilesUpload

The GET request is handled by a function called `handleTop'` that gets
the route for the `index.html` file using the `Paths` module and
serves it:

> handleTop' :: Snap ()
> handleTop' = do
>   indexHtml <- MonadIO.liftIO (Paths.getDataFileName "index.html")
>   SnapFileServe.serveFile indexHtml

The POST request is handled by the `handleFilesUpload` function, which
calls a function named `handleFilesUpload'` and writes the response
(either an error message or the contents of the uploaded files):

> handleFilesUpload :: Snap ()
> handleFilesUpload = do
>   eitherFilesUpload <- handleFilesUpload'
>   case eitherFilesUpload of
>     Left badRequestReason -> do
>       SnapCore.modifyResponse
>         (SnapCore.setResponseStatus 400 "Bad Request")
>       SnapCore.writeBS (ByteStringChar8.pack badRequestReason)
>     Right response ->
>       SnapCore.writeBS response

The `handleFilesUpload'` function uses Snap's [`handleFileUploads`][2]
function, which reads uploaded files to a temporary directory based on
general and per-file upload policies and uses a given handler to
actually do something with the files. Here, we choose to return either
a `String` (an error) or a `ByteString` (the contents of the uploaded
files):

> handleFilesUpload' :: Snap (Either String ByteString)
> handleFilesUpload' = do
>   temporaryDirectory <- MonadIO.liftIO Directory.getTemporaryDirectory
>   SnapFileUploads.handleFileUploads
>     temporaryDirectory
>     uploadPolicy
>     partUploadPolicy
>     handleFilesRead

As temporary directory, we'll simply use the system's temporary
directory.

The general upload policy allows us to specify things like the maximum
number of form inputs and the minimum upload rate. Snap provides a
default upload policy, which is good enough for our application:

> uploadPolicy :: UploadPolicy
> uploadPolicy =
>   SnapFileUploads.defaultUploadPolicy

With the part or per-file upload policy, we allow or disallow each
submitted file. Given a [`PartInfo`][3], we either disallow a file to
be uploaded or allow it with a maximum size. A value of `PartInfo`
gives us information such as the content type of the file and the name
of the file (which is optional and will be `Nothing` if we submit no
files):

> partUploadPolicy
>   :: PartInfo
>   -> PartUploadPolicy
> partUploadPolicy PartInfo{..} =
>   if (partContentType == "text/x-haskell"
>        || partContentType == "text/x-literate-haskell")
>        && Maybe.isJust partFileName
>      then
>        SnapFileUploads.allowWithMaximumSize maximumSize
>      else
>        SnapFileUploads.disallow

We only allow Haskell files (`.hs` or `.lhs`) with a name (no empty
submissions) with a maximum size of one megabyte:

> maximumSize :: Int64
> maximumSize =
>   1 * megabyte
>   where
>     megabyte =
>       2 ^ (20 :: Int)

Note that this part upload policy is not very precise. We could rename
a `turbulent-sniffle.pdf` file to `turbulent-sniffle.hs` and upload
it. Also, the content type of a Haskell file could be submitted as
`application/octet-stream`, which our upload policy would reject.

At last, we can implement the `handleFilesRead` and actually do
something with the uploaded files. This handler takes a list of
`PartInfo`s and file paths (or policy violations) and returns either
an error or the contents of the files:

> handleFilesRead
>   :: [(PartInfo, Either PolicyViolationException FilePath)]
>   -> Snap (Either String ByteString)
> handleFilesRead partInfos = do
>   eitherContents <- mapM handleFileRead (fmap snd partInfos)
>   case Either.partitionEithers eitherContents of
>     ([], contents) ->
>       return (Right (ByteStringChar8.concat contents))
>     (errors, _) -> do
>       SnapCore.logError
>         (ByteStringChar8.pack (unlines errors))
>       return (Left (head errors))

We already used the `PartInfo`s to allow or disallow files, so we
ignore it here and map over the file paths to get their contents using
the `handleFileRead` function. If there are no policy violations in
the results, we concatenate the contents of all files. Otherwise, we
log the errors and return the error message for the first file that
violated our upload policies.

Finally, let's define `handleFileRead` to read the contents of each
file (if the file is rejected, we just show the policy violation
message):

> handleFileRead
>   :: Either PolicyViolationException FilePath
>   -> Snap (Either String ByteString)
> handleFileRead eitherFile =
>   case eitherFile of
>     Left policyViolation ->
>       return (Left (show policyViolation))
>     Right file ->
>       MonadIO.liftIO (fmap Right (ByteStringChar8.readFile file))

\begin{quote}
Witchy kitchy kitchy wee,\\
Spikky wikky mikky bee,\\
Chippy wippy chee!”
\end{quote}
&mdash;Edward Lear ([Nonsense Books](http://www.gutenberg.org/ebooks/13650))

[haskell]: https://www.haskell.org/
[lts]: https://www.stackage.org/lts-3.18
[nightly]: https://www.stackage.org/nightly-2015-12-13
[snap]: http://snapframework.com/
[snap-core]: https://hackage.haskell.org/package/snap-core-0.9.8.0
[snap-server]: https://hackage.haskell.org/package/snap-server-0.9.5.1
[turbulent-sniffle]: https://github.com/stackbuilders/turbulent-sniffle

[1]: https://hackage.haskell.org/package/snap-core-0.9.8.0/docs/Snap-Util-FileUploads.html
[2]: https://hackage.haskell.org/package/snap-core-0.9.8.0/docs/Snap-Util-FileUploads.html#v:handleFileUploads
[3]: https://hackage.haskell.org/package/snap-core-0.9.8.0/docs/Snap-Util-FileUploads.html#t:PartInfo
