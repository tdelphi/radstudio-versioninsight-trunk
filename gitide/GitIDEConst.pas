{******************************************************************************}
{                                                                              }
{ RAD Studio Version Insight                                                   }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License          }
{ Version 1.1 (the "License"); you may not use this file except in compliance  }
{ with the License. You may obtain a copy of the License at                    }
{ http://www.mozilla.org/MPL/                                                  }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is GitIDEConst.pas.                                        }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2011 - 2014 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit GitIDEConst;

interface

resourcestring
  sGit = 'Git';
  sPMMGitParent = 'Git';
  sPMMCommit = 'Commit';
  sPMMUpdate = 'Pull';
  sPMMClean = 'Clean';
  sPMMPush = 'Push';
  sPMMLog = 'Show Log';
  sPMMRootDir = 'From Repository Root';
  sPMMProjectDir = 'From Project Directory';
  sPMMExpicitFiles = 'Files in this Project';
  sPMMRevert = 'Revert';
  sMenuOpenFromVersionControl = 'Open From Git (Clone)';

  sGitClientNotConfigured = 'Git client has not been properly configured. ' +#10+#13 + 'Go to Tools | Options | Version Control | Git page.';
  sCommit = 'Commit';
  sUnknownUserName = 'Unknown user name';
  sUnknownUserEmail = 'Unknown user email';
  sCommitCompleted = 'Commit completed at: %s';
  sUnknownError = 'Unknown Error';
  sCanceledByUser = 'Canceled by user';
  sRemoteRepositoryNotFound = 'Remote repository not found. %s';
  sVersionControlAddInOptionArea = 'Version Control';
  sLog = 'Log';
  sImport = 'Import';
  sWorking = '-Working';
  sCleaning = 'Cleaning ';
  sPushing = 'Pushing ';
  sUpdating = 'Updating ';

  SAuthor = 'Author: ';
  STime = 'Time: ';
  SComment = 'Comment: ';

  sCommitLoaded = 'A commit window is still open. Please close it if you wish to start a new commit.';
  sRevertedFile = 'Reverted: %s';
  sRetrievingFileRevision = 'Retrieving %s revision %s';
  sSavingFileRevision = 'Saving %s revision %s';



const
  // string errors to be parsed
  sAuthenticationFailed = 'Authentication failed';                // Do not localize
  sUnableToAccess = 'unable to access';                           // Do not localize
  sRepositoryNotFound = 'Repository not found';                   // Do not localize
  sInvalidUserNameOrPassword = 'Invalid user name or password';   // Do not localize

  // Context Ids reserved in Context.pas file
  { 15401 - 16000 For Git Integeration}
  hcGitClone                             = 15401;
  hcGitUpdateDlg                         = 15402;
  hcGitOptions                           = 15403;
  hcGitUnknownUserIdentity               = 15405;
  hcGitCommitView                        = 15406;

  {$I ..\delphisvn.inc}

  sGitRegBaseKey = '\VersionInsight\Git\RepoInfo\';
  sRemoteUrl = 'RemoteUrl';
  sUserName = 'UserName';
  sPassword = 'Password';


implementation

end.
