-- Channels
--type GitServer  : SU = *?GitService
type GitService : SL = +{ Push:        CommitC; GitService
                        , Pull: dualof CommitC; GitService
                        , Close: Skip
                        }

type CommitListC : SL = +{ Cons: CommitC; CommitListC
                         , Nil : Skip
                         }

type CommitC : SL = !CommitMessage; FileChangeListC

type FileChangeListC : SL = +{ Cons: FileChangeC; FileChangeListC
                             , Nil : Skip
                             }

type FileChangeC : SL = !FileName; ChangeListC

type ChangeListC : SL = +{ Cons: ChangeC; ChangeListC
                         , Nil : Skip
                         }

type ChangeC : SL = ChangeTypeC; !LineNumber; ContentC

type ChangeTypeC : SL = +{Add: Skip, Update: Skip, Remove: Skip}

type ContentC : SL = +{ Cons: !Line; ContentC
                      , Nil : Skip
                      }

-- Client
type ClientGit = (Staging, Repo)

-- Server
type ServerGit = Repo


-- Structures
type Staging    = ListFileChange
type FileChange = (FileName, ListChange)
type Change     = (ChangeType, LineNumber, Content)
type Content    = ListLine
data ChangeType = Add | Update | Remove
type Repo       = ListCommit
type Commit     = (CommitMessage, ListFileChange)

type FileName      = String
type Line          = String 
type LineNumber    = Int
type CommitMessage = String

-- Lists
data ListFileChange = ListFileChangeNil | ListFileChangeCons FileChange ListFileChange 
data ListChange     = ListChangeNil     | ListChangeCons     Change     ListChange
data ListLine       = ListLineNil       | ListLineCons       Line       ListLine 
data ListCommit     = ListCommitNil     | ListCommitCons     Commit     ListCommit