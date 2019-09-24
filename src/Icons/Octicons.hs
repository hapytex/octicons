-- vim:ft=haskell

{-# LANGUAGE OverloadedStrings #-}

module Icons.Octicons where

import Prelude hiding (span)
import Data.String(IsString(fromString))
import Icons(Icon(iconName))
import Text.Blaze(ToMarkup(toMarkup), (!))
import Text.Blaze.Html5(span)
import Text.Blaze.Html5.Attributes(class_)
import Text.Blaze.Internal(MarkupM(AddAttribute))

newtype Octicon = Octicon { octiconName :: String } deriving (Show, Read, Eq, Ord)

instance IsString Octicon where
    fromString = Octicon

instance Icon Octicon where
    iconName = octiconName

instance ToMarkup Octicon where
      toMarkup (Octicon a) = (span ! class_ (fromString ("octicon octicon-" <> a))) ""

-- | The alert octicon.
alert :: Octicon
alert = Octicon "alert"

-- | The archive octicon.
archive :: Octicon
archive = Octicon "archive"

-- | The arrow-both octicon.
arrowBoth :: Octicon
arrowBoth = Octicon "arrow-both"

-- | The arrow-down octicon.
arrowDown :: Octicon
arrowDown = Octicon "arrow-down"

-- | The arrow-left octicon.
arrowLeft :: Octicon
arrowLeft = Octicon "arrow-left"

-- | The arrow-right octicon.
arrowRight :: Octicon
arrowRight = Octicon "arrow-right"

-- | The arrow-small-down octicon.
arrowSmallDown :: Octicon
arrowSmallDown = Octicon "arrow-small-down"

-- | The arrow-small-left octicon.
arrowSmallLeft :: Octicon
arrowSmallLeft = Octicon "arrow-small-left"

-- | The arrow-small-right octicon.
arrowSmallRight :: Octicon
arrowSmallRight = Octicon "arrow-small-right"

-- | The arrow-small-up octicon.
arrowSmallUp :: Octicon
arrowSmallUp = Octicon "arrow-small-up"

-- | The arrow-up octicon.
arrowUp :: Octicon
arrowUp = Octicon "arrow-up"

-- | The beaker octicon.
beaker :: Octicon
beaker = Octicon "beaker"

-- | The bell octicon.
bell :: Octicon
bell = Octicon "bell"

-- | The bold octicon.
bold :: Octicon
bold = Octicon "bold"

-- | The book octicon.
book :: Octicon
book = Octicon "book"

-- | The bookmark octicon.
bookmark :: Octicon
bookmark = Octicon "bookmark"

-- | The briefcase octicon.
briefcase :: Octicon
briefcase = Octicon "briefcase"

-- | The broadcast octicon.
broadcast :: Octicon
broadcast = Octicon "broadcast"

-- | The browser octicon.
browser :: Octicon
browser = Octicon "browser"

-- | The bug octicon.
bug :: Octicon
bug = Octicon "bug"

-- | The calendar octicon.
calendar :: Octicon
calendar = Octicon "calendar"

-- | The check octicon.
check :: Octicon
check = Octicon "check"

-- | The checklist octicon.
checklist :: Octicon
checklist = Octicon "checklist"

-- | The chevron-down octicon.
chevronDown :: Octicon
chevronDown = Octicon "chevron-down"

-- | The chevron-left octicon.
chevronLeft :: Octicon
chevronLeft = Octicon "chevron-left"

-- | The chevron-right octicon.
chevronRight :: Octicon
chevronRight = Octicon "chevron-right"

-- | The chevron-up octicon.
chevronUp :: Octicon
chevronUp = Octicon "chevron-up"

-- | The circle-slash octicon.
circleSlash :: Octicon
circleSlash = Octicon "circle-slash"

-- | The circuit-board octicon.
circuitBoard :: Octicon
circuitBoard = Octicon "circuit-board"

-- | The clippy octicon.
clippy :: Octicon
clippy = Octicon "clippy"

-- | The clock octicon.
clock :: Octicon
clock = Octicon "clock"

-- | The cloud-download octicon.
cloudDownload :: Octicon
cloudDownload = Octicon "cloud-download"

-- | The cloud-upload octicon.
cloudUpload :: Octicon
cloudUpload = Octicon "cloud-upload"

-- | The code octicon.
code :: Octicon
code = Octicon "code"

-- | The comment octicon.
comment :: Octicon
comment = Octicon "comment"

-- | The comment-discussion octicon.
commentDiscussion :: Octicon
commentDiscussion = Octicon "comment-discussion"

-- | The credit-card octicon.
creditCard :: Octicon
creditCard = Octicon "credit-card"

-- | The dash octicon.
dash :: Octicon
dash = Octicon "dash"

-- | The dashboard octicon.
dashboard :: Octicon
dashboard = Octicon "dashboard"

-- | The database octicon.
database :: Octicon
database = Octicon "database"

-- | The dependent octicon.
dependent :: Octicon
dependent = Octicon "dependent"

-- | The desktop-download octicon.
desktopDownload :: Octicon
desktopDownload = Octicon "desktop-download"

-- | The device-camera octicon.
deviceCamera :: Octicon
deviceCamera = Octicon "device-camera"

-- | The device-camera-video octicon.
deviceCameraVideo :: Octicon
deviceCameraVideo = Octicon "device-camera-video"

-- | The device-desktop octicon.
deviceDesktop :: Octicon
deviceDesktop = Octicon "device-desktop"

-- | The device-mobile octicon.
deviceMobile :: Octicon
deviceMobile = Octicon "device-mobile"

-- | The diff octicon.
diff :: Octicon
diff = Octicon "diff"

-- | The diff-added octicon.
diffAdded :: Octicon
diffAdded = Octicon "diff-added"

-- | The diff-ignored octicon.
diffIgnored :: Octicon
diffIgnored = Octicon "diff-ignored"

-- | The diff-modified octicon.
diffModified :: Octicon
diffModified = Octicon "diff-modified"

-- | The diff-removed octicon.
diffRemoved :: Octicon
diffRemoved = Octicon "diff-removed"

-- | The diff-renamed octicon.
diffRenamed :: Octicon
diffRenamed = Octicon "diff-renamed"

-- | The ellipsis octicon.
ellipsis :: Octicon
ellipsis = Octicon "ellipsis"

-- | The eye octicon.
eye :: Octicon
eye = Octicon "eye"

-- | The eye-closed octicon.
eyeClosed :: Octicon
eyeClosed = Octicon "eye-closed"

-- | The file octicon.
file :: Octicon
file = Octicon "file"

-- | The file-binary octicon.
fileBinary :: Octicon
fileBinary = Octicon "file-binary"

-- | The file-code octicon.
fileCode :: Octicon
fileCode = Octicon "file-code"

-- | The file-directory octicon.
fileDirectory :: Octicon
fileDirectory = Octicon "file-directory"

-- | The file-media octicon.
fileMedia :: Octicon
fileMedia = Octicon "file-media"

-- | The file-pdf octicon.
filePdf :: Octicon
filePdf = Octicon "file-pdf"

-- | The file-submodule octicon.
fileSubmodule :: Octicon
fileSubmodule = Octicon "file-submodule"

-- | The file-symlink-directory octicon.
fileSymlinkDirectory :: Octicon
fileSymlinkDirectory = Octicon "file-symlink-directory"

-- | The file-symlink-file octicon.
fileSymlinkFile :: Octicon
fileSymlinkFile = Octicon "file-symlink-file"

-- | The file-zip octicon.
fileZip :: Octicon
fileZip = Octicon "file-zip"

-- | The flame octicon.
flame :: Octicon
flame = Octicon "flame"

-- | The fold octicon.
fold :: Octicon
fold = Octicon "fold"

-- | The fold-down octicon.
foldDown :: Octicon
foldDown = Octicon "fold-down"

-- | The fold-up octicon.
foldUp :: Octicon
foldUp = Octicon "fold-up"

-- | The gear octicon.
gear :: Octicon
gear = Octicon "gear"

-- | The gift octicon.
gift :: Octicon
gift = Octicon "gift"

-- | The gist octicon.
gist :: Octicon
gist = Octicon "gist"

-- | The gist-secret octicon.
gistSecret :: Octicon
gistSecret = Octicon "gist-secret"

-- | The git-branch octicon.
gitBranch :: Octicon
gitBranch = Octicon "git-branch"

-- | The git-commit octicon.
gitCommit :: Octicon
gitCommit = Octicon "git-commit"

-- | The git-compare octicon.
gitCompare :: Octicon
gitCompare = Octicon "git-compare"

-- | The github-action octicon.
githubAction :: Octicon
githubAction = Octicon "github-action"

-- | The git-merge octicon.
gitMerge :: Octicon
gitMerge = Octicon "git-merge"

-- | The git-pull-request octicon.
gitPullRequest :: Octicon
gitPullRequest = Octicon "git-pull-request"

-- | The globe octicon.
globe :: Octicon
globe = Octicon "globe"

-- | The grabber octicon.
grabber :: Octicon
grabber = Octicon "grabber"

-- | The graph octicon.
graph :: Octicon
graph = Octicon "graph"

-- | The heart octicon.
heart :: Octicon
heart = Octicon "heart"

-- | The history octicon.
history :: Octicon
history = Octicon "history"

-- | The home octicon.
home :: Octicon
home = Octicon "home"

-- | The horizontal-rule octicon.
horizontalRule :: Octicon
horizontalRule = Octicon "horizontal-rule"

-- | The hubot octicon.
hubot :: Octicon
hubot = Octicon "hubot"

-- | The inbox octicon.
inbox :: Octicon
inbox = Octicon "inbox"

-- | The info octicon.
info :: Octicon
info = Octicon "info"

-- | The issue-closed octicon.
issueClosed :: Octicon
issueClosed = Octicon "issue-closed"

-- | The issue-opened octicon.
issueOpened :: Octicon
issueOpened = Octicon "issue-opened"

-- | The issue-reopened octicon.
issueReopened :: Octicon
issueReopened = Octicon "issue-reopened"

-- | The italic octicon.
italic :: Octicon
italic = Octicon "italic"

-- | The jersey octicon.
jersey :: Octicon
jersey = Octicon "jersey"

-- | The kebab-horizontal octicon.
kebabHorizontal :: Octicon
kebabHorizontal = Octicon "kebab-horizontal"

-- | The kebab-vertical octicon.
kebabVertical :: Octicon
kebabVertical = Octicon "kebab-vertical"

-- | The key octicon.
key :: Octicon
key = Octicon "key"

-- | The keyboard octicon.
keyboard :: Octicon
keyboard = Octicon "keyboard"

-- | The law octicon.
law :: Octicon
law = Octicon "law"

-- | The light-bulb octicon.
lightBulb :: Octicon
lightBulb = Octicon "light-bulb"

-- | The link octicon.
link :: Octicon
link = Octicon "link"

-- | The link-external octicon.
linkExternal :: Octicon
linkExternal = Octicon "link-external"

-- | The list-ordered octicon.
listOrdered :: Octicon
listOrdered = Octicon "list-ordered"

-- | The list-unordered octicon.
listUnordered :: Octicon
listUnordered = Octicon "list-unordered"

-- | The location octicon.
location :: Octicon
location = Octicon "location"

-- | The lock octicon.
lock :: Octicon
lock = Octicon "lock"

-- | The logo-gist octicon.
logoGist :: Octicon
logoGist = Octicon "logo-gist"

-- | The logo-github octicon.
logoGithub :: Octicon
logoGithub = Octicon "logo-github"

-- | The mail octicon.
mail :: Octicon
mail = Octicon "mail"

-- | The mail-read octicon.
mailRead :: Octicon
mailRead = Octicon "mail-read"

-- | The markdown octicon.
markdown :: Octicon
markdown = Octicon "markdown"

-- | The mark-github octicon.
markGithub :: Octicon
markGithub = Octicon "mark-github"

-- | The megaphone octicon.
megaphone :: Octicon
megaphone = Octicon "megaphone"

-- | The mention octicon.
mention :: Octicon
mention = Octicon "mention"

-- | The milestone octicon.
milestone :: Octicon
milestone = Octicon "milestone"

-- | The mirror octicon.
mirror :: Octicon
mirror = Octicon "mirror"

-- | The mortar-board octicon.
mortarBoard :: Octicon
mortarBoard = Octicon "mortar-board"

-- | The mute octicon.
mute :: Octicon
mute = Octicon "mute"

-- | The no-newline octicon.
noNewline :: Octicon
noNewline = Octicon "no-newline"

-- | The note octicon.
note :: Octicon
note = Octicon "note"

-- | The octoface octicon.
octoface :: Octicon
octoface = Octicon "octoface"

-- | The organization octicon.
organization :: Octicon
organization = Octicon "organization"

-- | The package octicon.
package :: Octicon
package = Octicon "package"

-- | The paintcan octicon.
paintcan :: Octicon
paintcan = Octicon "paintcan"

-- | The pencil octicon.
pencil :: Octicon
pencil = Octicon "pencil"

-- | The person octicon.
person :: Octicon
person = Octicon "person"

-- | The pin octicon.
pin :: Octicon
pin = Octicon "pin"

-- | The play octicon.
play :: Octicon
play = Octicon "play"

-- | The plug octicon.
plug :: Octicon
plug = Octicon "plug"

-- | The plus octicon.
plus :: Octicon
plus = Octicon "plus"

-- | The plus-small octicon.
plusSmall :: Octicon
plusSmall = Octicon "plus-small"

-- | The primitive-dot octicon.
primitiveDot :: Octicon
primitiveDot = Octicon "primitive-dot"

-- | The primitive-square octicon.
primitiveSquare :: Octicon
primitiveSquare = Octicon "primitive-square"

-- | The project octicon.
project :: Octicon
project = Octicon "project"

-- | The pulse octicon.
pulse :: Octicon
pulse = Octicon "pulse"

-- | The question octicon.
question :: Octicon
question = Octicon "question"

-- | The quote octicon.
quote :: Octicon
quote = Octicon "quote"

-- | The radio-tower octicon.
radioTower :: Octicon
radioTower = Octicon "radio-tower"

-- | The reply octicon.
reply :: Octicon
reply = Octicon "reply"

-- | The repo octicon.
repo :: Octicon
repo = Octicon "repo"

-- | The repo-clone octicon.
repoClone :: Octicon
repoClone = Octicon "repo-clone"

-- | The repo-force-push octicon.
repoForcePush :: Octicon
repoForcePush = Octicon "repo-force-push"

-- | The repo-forked octicon.
repoForked :: Octicon
repoForked = Octicon "repo-forked"

-- | The repo-pull octicon.
repoPull :: Octicon
repoPull = Octicon "repo-pull"

-- | The repo-push octicon.
repoPush :: Octicon
repoPush = Octicon "repo-push"

-- | The report octicon.
report :: Octicon
report = Octicon "report"

-- | The repo-template octicon.
repoTemplate :: Octicon
repoTemplate = Octicon "repo-template"

-- | The repo-template-private octicon.
repoTemplatePrivate :: Octicon
repoTemplatePrivate = Octicon "repo-template-private"

-- | The request-changes octicon.
requestChanges :: Octicon
requestChanges = Octicon "request-changes"

-- | The rocket octicon.
rocket :: Octicon
rocket = Octicon "rocket"

-- | The rss octicon.
rss :: Octicon
rss = Octicon "rss"

-- | The ruby octicon.
ruby :: Octicon
ruby = Octicon "ruby"

-- | The screen-full octicon.
screenFull :: Octicon
screenFull = Octicon "screen-full"

-- | The screen-normal octicon.
screenNormal :: Octicon
screenNormal = Octicon "screen-normal"

-- | The search octicon.
search :: Octicon
search = Octicon "search"

-- | The server octicon.
server :: Octicon
server = Octicon "server"

-- | The settings octicon.
settings :: Octicon
settings = Octicon "settings"

-- | The shield octicon.
shield :: Octicon
shield = Octicon "shield"

-- | The shield-check octicon.
shieldCheck :: Octicon
shieldCheck = Octicon "shield-check"

-- | The shield-lock octicon.
shieldLock :: Octicon
shieldLock = Octicon "shield-lock"

-- | The shield-x octicon.
shieldX :: Octicon
shieldX = Octicon "shield-x"

-- | The sign-in octicon.
signIn :: Octicon
signIn = Octicon "sign-in"

-- | The sign-out octicon.
signOut :: Octicon
signOut = Octicon "sign-out"

-- | The skip octicon.
skip :: Octicon
skip = Octicon "skip"

-- | The smiley octicon.
smiley :: Octicon
smiley = Octicon "smiley"

-- | The squirrel octicon.
squirrel :: Octicon
squirrel = Octicon "squirrel"

-- | The star octicon.
star :: Octicon
star = Octicon "star"

-- | The stop octicon.
stop :: Octicon
stop = Octicon "stop"

-- | The sync octicon.
sync :: Octicon
sync = Octicon "sync"

-- | The tag octicon.
tag :: Octicon
tag = Octicon "tag"

-- | The tasklist octicon.
tasklist :: Octicon
tasklist = Octicon "tasklist"

-- | The telescope octicon.
telescope :: Octicon
telescope = Octicon "telescope"

-- | The terminal octicon.
terminal :: Octicon
terminal = Octicon "terminal"

-- | The text-size octicon.
textSize :: Octicon
textSize = Octicon "text-size"

-- | The three-bars octicon.
threeBars :: Octicon
threeBars = Octicon "three-bars"

-- | The thumbsdown octicon.
thumbsdown :: Octicon
thumbsdown = Octicon "thumbsdown"

-- | The thumbsup octicon.
thumbsup :: Octicon
thumbsup = Octicon "thumbsup"

-- | The tools octicon.
tools :: Octicon
tools = Octicon "tools"

-- | The trashcan octicon.
trashcan :: Octicon
trashcan = Octicon "trashcan"

-- | The triangle-down octicon.
triangleDown :: Octicon
triangleDown = Octicon "triangle-down"

-- | The triangle-left octicon.
triangleLeft :: Octicon
triangleLeft = Octicon "triangle-left"

-- | The triangle-right octicon.
triangleRight :: Octicon
triangleRight = Octicon "triangle-right"

-- | The triangle-up octicon.
triangleUp :: Octicon
triangleUp = Octicon "triangle-up"

-- | The unfold octicon.
unfold :: Octicon
unfold = Octicon "unfold"

-- | The unmute octicon.
unmute :: Octicon
unmute = Octicon "unmute"

-- | The unverified octicon.
unverified :: Octicon
unverified = Octicon "unverified"

-- | The verified octicon.
verified :: Octicon
verified = Octicon "verified"

-- | The versions octicon.
versions :: Octicon
versions = Octicon "versions"

-- | The watch octicon.
watch :: Octicon
watch = Octicon "watch"

-- | The x octicon.
x :: Octicon
x = Octicon "x"

-- | The zap octicon.
zap :: Octicon
zap = Octicon "zap"
