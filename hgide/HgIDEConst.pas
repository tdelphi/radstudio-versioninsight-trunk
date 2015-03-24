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
{ The Original Code is HgIDEConst.pas.                                         }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2011 - 2014 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit HgIDEConst;

interface

resourcestring
  sMercurial = 'Mercurial';
  sPMMHgParent = 'Mercurial';
  sPMMCommit = 'Commit';
  sUnknownUserName = 'Unknown user name';
  sUnknownUserEmail = 'Unknown user email';
  sPMMLog = 'Show Log';
  sPMMRootDir = 'From Repository Root';
  sPMMProjectDir = 'From Project Directory';
  sPMMExpicitFiles = 'Files in this Project';
  sPMMRevert = 'Revert';
  sHgClientNotConfigured = 'Mercurial client has not been properly configured. ' +#10+#13 + 'Go to Tools | Options | Version Control | Mercurial page.';
  sMenuOpenFromVersionControl = 'Open From Mercurial (Clone)';
  sVersionControlAddInOptionArea = 'Version Control';
  sCommit = 'Commit';
  sCommitCompleted = 'Commit completed at revision: %d';
  sLog = 'Log';
  sWorking = '-Working';

  SAuthor = 'Author: ';
  STime = 'Time: ';
  SComment = 'Comment: ';

  sCommitLoaded = 'A commit window is still open. Please close it if you wish to start a new commit.';
  sRevertedFile = 'Reverted: %s';
  sRetrievingFileRevision = 'Retrieving %s revision %d';
  sSavingFileRevision = 'Saving %s revision %d';

const
 // Context Ids reserved in Context.pas file
{ 15500 - 16000 For Mercurial Integeration}
  hcHgClone                              = 15501;
  hcHgUpdateDlg                          = 15502;
  hcHgOptions                            = 15503;
  hcHgUnknownUserIdentity                = 15505;
  hcHgCommitView                         = 15506;

  {$I ..\delphisvn.inc}

implementation

end.
