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

alert :: Octicon
alert = Octicon "alert"

archive :: Octicon
archive = Octicon "archive"

arrowBoth :: Octicon
arrowBoth = Octicon "arrow-both"

arrowDown :: Octicon
arrowDown = Octicon "arrow-down"

arrowLeft :: Octicon
arrowLeft = Octicon "arrow-left"

arrowRight :: Octicon
arrowRight = Octicon "arrow-right"

arrowSmallDown :: Octicon
arrowSmallDown = Octicon "arrow-small-down"

arrowSmallLeft :: Octicon
arrowSmallLeft = Octicon "arrow-small-left"

arrowSmallRight :: Octicon
arrowSmallRight = Octicon "arrow-small-right"

arrowSmallUp :: Octicon
arrowSmallUp = Octicon "arrow-small-up"

arrowUp :: Octicon
arrowUp = Octicon "arrow-up"

beaker :: Octicon
beaker = Octicon "beaker"

bell :: Octicon
bell = Octicon "bell"

bold :: Octicon
bold = Octicon "bold"

book :: Octicon
book = Octicon "book"

bookmark :: Octicon
bookmark = Octicon "bookmark"

briefcase :: Octicon
briefcase = Octicon "briefcase"

broadcast :: Octicon
broadcast = Octicon "broadcast"

browser :: Octicon
browser = Octicon "browser"

bug :: Octicon
bug = Octicon "bug"

calendar :: Octicon
calendar = Octicon "calendar"

check :: Octicon
check = Octicon "check"

checklist :: Octicon
checklist = Octicon "checklist"

chevronDown :: Octicon
chevronDown = Octicon "chevron-down"

chevronLeft :: Octicon
chevronLeft = Octicon "chevron-left"

chevronRight :: Octicon
chevronRight = Octicon "chevron-right"

chevronUp :: Octicon
chevronUp = Octicon "chevron-up"

circleSlash :: Octicon
circleSlash = Octicon "circle-slash"

circuitBoard :: Octicon
circuitBoard = Octicon "circuit-board"

clippy :: Octicon
clippy = Octicon "clippy"

clock :: Octicon
clock = Octicon "clock"

cloudDownload :: Octicon
cloudDownload = Octicon "cloud-download"

cloudUpload :: Octicon
cloudUpload = Octicon "cloud-upload"

code :: Octicon
code = Octicon "code"

comment :: Octicon
comment = Octicon "comment"

commentDiscussion :: Octicon
commentDiscussion = Octicon "comment-discussion"

creditCard :: Octicon
creditCard = Octicon "credit-card"

dash :: Octicon
dash = Octicon "dash"

dashboard :: Octicon
dashboard = Octicon "dashboard"

database :: Octicon
database = Octicon "database"

dependent :: Octicon
dependent = Octicon "dependent"

desktopDownload :: Octicon
desktopDownload = Octicon "desktop-download"

deviceCamera :: Octicon
deviceCamera = Octicon "device-camera"

deviceCameraVideo :: Octicon
deviceCameraVideo = Octicon "device-camera-video"

deviceDesktop :: Octicon
deviceDesktop = Octicon "device-desktop"

deviceMobile :: Octicon
deviceMobile = Octicon "device-mobile"

diff :: Octicon
diff = Octicon "diff"

diffAdded :: Octicon
diffAdded = Octicon "diff-added"

diffIgnored :: Octicon
diffIgnored = Octicon "diff-ignored"

diffModified :: Octicon
diffModified = Octicon "diff-modified"

diffRemoved :: Octicon
diffRemoved = Octicon "diff-removed"

diffRenamed :: Octicon
diffRenamed = Octicon "diff-renamed"

ellipsis :: Octicon
ellipsis = Octicon "ellipsis"

eye :: Octicon
eye = Octicon "eye"

eyeClosed :: Octicon
eyeClosed = Octicon "eye-closed"

file :: Octicon
file = Octicon "file"

fileBinary :: Octicon
fileBinary = Octicon "file-binary"

fileCode :: Octicon
fileCode = Octicon "file-code"

fileDirectory :: Octicon
fileDirectory = Octicon "file-directory"

fileMedia :: Octicon
fileMedia = Octicon "file-media"

filePdf :: Octicon
filePdf = Octicon "file-pdf"

fileSubmodule :: Octicon
fileSubmodule = Octicon "file-submodule"

fileSymlinkDirectory :: Octicon
fileSymlinkDirectory = Octicon "file-symlink-directory"

fileSymlinkFile :: Octicon
fileSymlinkFile = Octicon "file-symlink-file"

fileZip :: Octicon
fileZip = Octicon "file-zip"

flame :: Octicon
flame = Octicon "flame"

fold :: Octicon
fold = Octicon "fold"

foldDown :: Octicon
foldDown = Octicon "fold-down"

foldUp :: Octicon
foldUp = Octicon "fold-up"

gear :: Octicon
gear = Octicon "gear"

gift :: Octicon
gift = Octicon "gift"

gist :: Octicon
gist = Octicon "gist"

gistSecret :: Octicon
gistSecret = Octicon "gist-secret"

gitBranch :: Octicon
gitBranch = Octicon "git-branch"

gitCommit :: Octicon
gitCommit = Octicon "git-commit"

gitCompare :: Octicon
gitCompare = Octicon "git-compare"

githubAction :: Octicon
githubAction = Octicon "github-action"

gitMerge :: Octicon
gitMerge = Octicon "git-merge"

gitPullRequest :: Octicon
gitPullRequest = Octicon "git-pull-request"

globe :: Octicon
globe = Octicon "globe"

grabber :: Octicon
grabber = Octicon "grabber"

graph :: Octicon
graph = Octicon "graph"

heart :: Octicon
heart = Octicon "heart"

history :: Octicon
history = Octicon "history"

home :: Octicon
home = Octicon "home"

horizontalRule :: Octicon
horizontalRule = Octicon "horizontal-rule"

hubot :: Octicon
hubot = Octicon "hubot"

inbox :: Octicon
inbox = Octicon "inbox"

info :: Octicon
info = Octicon "info"

issueClosed :: Octicon
issueClosed = Octicon "issue-closed"

issueOpened :: Octicon
issueOpened = Octicon "issue-opened"

issueReopened :: Octicon
issueReopened = Octicon "issue-reopened"

italic :: Octicon
italic = Octicon "italic"

jersey :: Octicon
jersey = Octicon "jersey"

kebabHorizontal :: Octicon
kebabHorizontal = Octicon "kebab-horizontal"

kebabVertical :: Octicon
kebabVertical = Octicon "kebab-vertical"

key :: Octicon
key = Octicon "key"

keyboard :: Octicon
keyboard = Octicon "keyboard"

law :: Octicon
law = Octicon "law"

lightBulb :: Octicon
lightBulb = Octicon "light-bulb"

link :: Octicon
link = Octicon "link"

linkExternal :: Octicon
linkExternal = Octicon "link-external"

listOrdered :: Octicon
listOrdered = Octicon "list-ordered"

listUnordered :: Octicon
listUnordered = Octicon "list-unordered"

location :: Octicon
location = Octicon "location"

lock :: Octicon
lock = Octicon "lock"

logoGist :: Octicon
logoGist = Octicon "logo-gist"

logoGithub :: Octicon
logoGithub = Octicon "logo-github"

mail :: Octicon
mail = Octicon "mail"

mailRead :: Octicon
mailRead = Octicon "mail-read"

markdown :: Octicon
markdown = Octicon "markdown"

markGithub :: Octicon
markGithub = Octicon "mark-github"

megaphone :: Octicon
megaphone = Octicon "megaphone"

mention :: Octicon
mention = Octicon "mention"

milestone :: Octicon
milestone = Octicon "milestone"

mirror :: Octicon
mirror = Octicon "mirror"

mortarBoard :: Octicon
mortarBoard = Octicon "mortar-board"

mute :: Octicon
mute = Octicon "mute"

noNewline :: Octicon
noNewline = Octicon "no-newline"

note :: Octicon
note = Octicon "note"

octoface :: Octicon
octoface = Octicon "octoface"

organization :: Octicon
organization = Octicon "organization"

package :: Octicon
package = Octicon "package"

paintcan :: Octicon
paintcan = Octicon "paintcan"

pencil :: Octicon
pencil = Octicon "pencil"

person :: Octicon
person = Octicon "person"

pin :: Octicon
pin = Octicon "pin"

play :: Octicon
play = Octicon "play"

plug :: Octicon
plug = Octicon "plug"

plus :: Octicon
plus = Octicon "plus"

plusSmall :: Octicon
plusSmall = Octicon "plus-small"

primitiveDot :: Octicon
primitiveDot = Octicon "primitive-dot"

primitiveSquare :: Octicon
primitiveSquare = Octicon "primitive-square"

project :: Octicon
project = Octicon "project"

pulse :: Octicon
pulse = Octicon "pulse"

question :: Octicon
question = Octicon "question"

quote :: Octicon
quote = Octicon "quote"

radioTower :: Octicon
radioTower = Octicon "radio-tower"

reply :: Octicon
reply = Octicon "reply"

repo :: Octicon
repo = Octicon "repo"

repoClone :: Octicon
repoClone = Octicon "repo-clone"

repoForcePush :: Octicon
repoForcePush = Octicon "repo-force-push"

repoForked :: Octicon
repoForked = Octicon "repo-forked"

repoPull :: Octicon
repoPull = Octicon "repo-pull"

repoPush :: Octicon
repoPush = Octicon "repo-push"

report :: Octicon
report = Octicon "report"

repoTemplate :: Octicon
repoTemplate = Octicon "repo-template"

repoTemplatePrivate :: Octicon
repoTemplatePrivate = Octicon "repo-template-private"

requestChanges :: Octicon
requestChanges = Octicon "request-changes"

rocket :: Octicon
rocket = Octicon "rocket"

rss :: Octicon
rss = Octicon "rss"

ruby :: Octicon
ruby = Octicon "ruby"

screenFull :: Octicon
screenFull = Octicon "screen-full"

screenNormal :: Octicon
screenNormal = Octicon "screen-normal"

search :: Octicon
search = Octicon "search"

server :: Octicon
server = Octicon "server"

settings :: Octicon
settings = Octicon "settings"

shield :: Octicon
shield = Octicon "shield"

shieldCheck :: Octicon
shieldCheck = Octicon "shield-check"

shieldLock :: Octicon
shieldLock = Octicon "shield-lock"

shieldX :: Octicon
shieldX = Octicon "shield-x"

signIn :: Octicon
signIn = Octicon "sign-in"

signOut :: Octicon
signOut = Octicon "sign-out"

skip :: Octicon
skip = Octicon "skip"

smiley :: Octicon
smiley = Octicon "smiley"

squirrel :: Octicon
squirrel = Octicon "squirrel"

star :: Octicon
star = Octicon "star"

stop :: Octicon
stop = Octicon "stop"

sync :: Octicon
sync = Octicon "sync"

tag :: Octicon
tag = Octicon "tag"

tasklist :: Octicon
tasklist = Octicon "tasklist"

telescope :: Octicon
telescope = Octicon "telescope"

terminal :: Octicon
terminal = Octicon "terminal"

textSize :: Octicon
textSize = Octicon "text-size"

threeBars :: Octicon
threeBars = Octicon "three-bars"

thumbsdown :: Octicon
thumbsdown = Octicon "thumbsdown"

thumbsup :: Octicon
thumbsup = Octicon "thumbsup"

tools :: Octicon
tools = Octicon "tools"

trashcan :: Octicon
trashcan = Octicon "trashcan"

triangleDown :: Octicon
triangleDown = Octicon "triangle-down"

triangleLeft :: Octicon
triangleLeft = Octicon "triangle-left"

triangleRight :: Octicon
triangleRight = Octicon "triangle-right"

triangleUp :: Octicon
triangleUp = Octicon "triangle-up"

unfold :: Octicon
unfold = Octicon "unfold"

unmute :: Octicon
unmute = Octicon "unmute"

unverified :: Octicon
unverified = Octicon "unverified"

verified :: Octicon
verified = Octicon "verified"

versions :: Octicon
versions = Octicon "versions"

watch :: Octicon
watch = Octicon "watch"

x :: Octicon
x = Octicon "x"

zap :: Octicon
zap = Octicon "zap"
