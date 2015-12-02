A turbulent sniffle
===

\begin{quote}
“I was also sad, and thinking,\\
When one day I saw you winking,\\
And I heard you sniffle-snuffle,\\
And I saw your feathers ruffle.
\end{quote}

Introduction. Uploading files is common for web applications. Snap is
a common framework in Haskell and has file uploading by default. This
functionality is really well thought and good documented, but lacks a
tutorial to show the basic idea of how to use it and integrate it to a
Snap application. Add links to the documentation and to the two Stack
Overflow questions related to file uploading with Snap.

We had to add file uploading to some Snap applications recently and
decided that it would be a good idea to write a tutorial on how to
create a form uploader. The idea is very simple: we'll build an
application that consists of a simple HTML form to upload multiple
Haskell files (hs or lhs). After submitting the files, the application
simply reads and prints the contents of all uploaded files.

The code for this blog posts is based on a literate file which you can
find in the turbulent-sniffle repository (link). As of writing, the
code was tested with LTS Haskell 3.16, that is, snap-core 0.9.8.0 and
snap-server 0.9.5.1. Add links to LTS, snap-core, and snap-server.

The first thing we'll do is write a very simple HTML form to handle
uploading files. Let's create a file index.html and add a form for this:

```html
<form action="/" method="post" enctype="multipart/form-data">
  <input type="file" name="files" multiple>
  <button type="submit">Submit</button>
</form>
```

This is a form with an input of type file with name files. The only
additional thing we add is multiple to handle multiple files. We could
make the input required or are more things, but we won't make any kind
of validation here just to make Haskell handle any kind of
problem. The only requirement to use Snap's functions is that we need
to make this a post method and the enctype has to be
multipart/form-data. Also, we choose / as the action for this posts,
which is the only route for this application.

The idea is to be able to use this in an existing application or
something like that, so let's create a Cabal file to list
dependencies. You can find a complete but minimal Cabal file in the
repository, but let's make one here for completeness.

```
name: turbulent-sniffle
version: 0.1.0
build-type: Simple
cabal-version: >= 1.22
```

This particular package is called turbulent-sniffle and we don't need
to change anything here, it should be given by the application that
needs to upload files.

We need to include data files because we're going to use HTML
directly. You could use blaze or something else in a real application:

```
data-files: index.html
```

Next, let's add an executable.

```
executable turbulent-sniffle
  main-is: Main.lhs
  other-modules: Paths_turbulent_sniffle
  ghc-options: -Wall -threaded -rtsopts -with-rtsopts=-N
  build-depends:
      base >= 4.8 && < 4.9
    , bytestring
    , directory
    , snap-core >= 0.9 && < 0.10
    , snap-server >= 0.9 && < 0.10
    , transformers
  default-language: Haskell2010
```

We're going to use only one file (Main.lhs) for the Haskell code, and
we include Paths to have access to the HTML file. The only non-basic
dependencies for the application will be snap-core and server.

That's it, that's all the setup we need. If you cloned the repository
or add the code and want to test, simply build and run the
application. If using Stack:

```
$ stack build --exec turbulent-sniffle
```

You can now go to http://localhost:8000/ and upload Haskell files. If
you cloned this repository, you can upload the source code (Main.lhs)
and read it instead.

\begin{quote}
To myself I sadly said,\\
‘She's neuralgia in her head!\\
That dear head has nothing on it!\\
Ought she not to wear a bonnet?’
\end{quote}

Finally, we can move to the Haskell code. First, some language
extensions and the module definition:

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE RecordWildCards #-}
>
> module Main
>  ( main
>  )
>  where

We only need to export the main function here to run the application,
but that's not the best idea.

Second, we need some imports, as follows. These are as explicit as
possible so that there's no doubt where things come from.

> -- turbulent-sniffle
> import qualified Paths_turbulent_sniffle as Paths
>
> -- base
> import Control.Applicative ((<|>))
> import qualified Data.Either as Either
> import Data.Int (Int64)
> import qualified Data.Maybe as Maybe
>
> -- bytestring
> import Data.ByteString (ByteString)
> import qualified Data.ByteString.Char8 as ByteString
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

snap-server allows us to quickly serve an application with
quickHttpServe. We can do that and simply print a ByteString to do
some kind of hello, world for Snap:

< main :: IO ()
< main =
<   SnapServer.quickHttpServe
<     (SnapCore.ifTop (SnapCore.writeBS "Behold a turbulent sniffle!"))

Instead of writing a bytestring, we want something else to handle the
top / route:

> main :: IO ()
> main =
>   SnapServer.quickHttpServe (SnapCore.ifTop handleTop)

handleTop will take care of a GET request to display the index.html
file and a POST request to actually upload the files, that is, to
handle the form submission. Thus, we specify this:

> handleTop :: Snap ()
> handleTop =
>   SnapCore.method GET handleTop'
>     <|> SnapCore.method POST handleFilesUpload

The GET request is handled by a function called handleTop' that gets
the route for the index.html file using the Paths module and serves
it:

> handleTop' :: Snap ()
> handleTop' = do
>   indexHtml <- MonadIO.liftIO (Paths.getDataFileName "index.html")
>   SnapFileServe.serveFile indexHtml

That's all that happens when you first go to the / route (localhost).

The real part is the handleFilesUpload function, which takes care of
the form submission. Or not, this is just a function that calls
another handler to actually handle the files uploading and either
writes a reason for failure or the response (which we already said is
the content of the uploaded files):

> handleFilesUpload :: Snap ()
> handleFilesUpload = do
>   eitherFilesUpload <- handleFilesUpload'
>   case eitherFilesUpload of
>     Left reason -> do
>       SnapCore.modifyResponse
>         (SnapCore.setResponseStatus 400 "Bad Request")
>       SnapCore.writeBS (ByteString.pack reason)
>     Right response ->
>       SnapCore.writeBS response

The real file uploading functionality comes from the handleFileUploads
function, which has good documentation (say something). We choose to
return an Either to signal success or error:

> handleFilesUpload' :: Snap (Either String ByteString)
> handleFilesUpload' = do
>   temporaryDirectory <- MonadIO.liftIO Directory.getTemporaryDirectory
>   SnapFileUploads.handleFileUploads
>     temporaryDirectory
>     uploadPolicy
>     partUploadPolicy
>     handleFilesRead

The handleFileUploads function takes four parameters: a temporary
directory to temporarily save the uploaded files (Snap removes them
after you do whatever you want to do with them), a general upload
policy, a per file upload policy and a function to actually handle the
uploaded files. Let's look at each of this in some detail.

For temporary files, we'll use the temporary directory as returned by
the directory package.

The upload policy gives you more things. Say something about it. For
this sample application, we'll use the default upload policy (say what
it is):

> uploadPolicy :: UploadPolicy
> uploadPolicy =
>   SnapFileUploads.defaultUploadPolicy

The part upload policy allows us to accept or reject a file. Given a
part info (say what it has) we can see the content type, whether it's
actually a file (we can click submit without choosing a file because
the input is not required). In this case, we only want Haskell or
literate Haskell files, and we require each file to be a file (we
handle submission with no files). If our policies are met, then we
allow files to be upladied with a maximum size. Else, we disallow the
file:

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

The maximum size is expressed like an Int64 in megabytes. We want one
megabyte in this case, which is more than enough:

> maximumSize :: Int64
> maximumSize =
>   1 * megabyte
>   where
>     megabyte =
>       2 ^ (20 :: Int)

Note that the part upload policy is not the best and you'd want to do
some extra work in a real application. As an example, you can take a
PDF file and change its extension from pdf to hs or lhs. If the file
is less than a megabyte, the part upload policy allows the file to be
uploaded. Even better, the application works and displays the PDF, but
that's not what you want.

Finally, the handleFilesRead function, the fourth parameter to Snap's
function, takes a list of files and here we can specify what to do
with them. This function takes a list of part infos and either a
policy violation exception or the temporary file path to each
file. We have to do something with each element in the list and return
an Either:

> handleFilesRead
>   :: [(PartInfo, Either PolicyViolationException FilePath)]
>   -> Snap (Either String ByteString)
> handleFilesRead pfs = do
>   sd <- mapM handleFileRead (fmap snd pfs)
>   case Either.partitionEithers sd of
>     ([], contents) ->
>       return (Right (ByteString.concat contents))
>     (errors, _) -> do
>       SnapCore.logError
>         (ByteString.pack (unlines errors))
>       return (Left (head errors))

In this case, we ignore the part infos and map a function over the
policy exceptions or file paths to read the files. After that, we only
return a success if all files were successfully uploaded (that is, if
partitioning the resulting list of eithers has no lefts). In that
case, we concatenate the content of all files. If there's at least one
error, we log the error and return an error with the first error
message. This is not very elaborate, so a different approach could be
taken.

The last piece of code is a function to handle each individual
file. This will simply take a file (or a policy violation exception)
and read the file. Here's where your application could do something
interesting with the file.

> handleFileRead
>   :: Either PolicyViolationException FilePath
>   -> Snap (Either String ByteString)
> handleFileRead =
>   either
>     (return . Left . show)
>     (MonadIO.liftIO . fmap Right . ByteString.readFile)

That's it. The Snap framework is really great and the file uploading
functionality has been designed with a lot of detail in mind. We hope
that this tutorial is useful in addition to the official
documentation.

\begin{quote}
Witchy kitchy kitchy wee,\\
Spikky wikky mikky bee,\\
Chippy wippy chee!”
\end{quote}
