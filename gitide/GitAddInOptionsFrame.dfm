object frmGitTestsOptions: TfrmGitTestsOptions
  Left = 0
  Top = 0
  Width = 469
  Height = 524
  HelpContext = 15403
  TabOrder = 0
  DesignSize = (
    469
    524)
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 448
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Git Options '
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 69
      Height = 13
      Caption = 'Git Executable'
    end
    object SpeedButton1: TSpeedButton
      Left = 363
      Top = 32
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = SpeedButton1Click
    end
    object edGitExecutable: TEdit
      Left = 8
      Top = 33
      Width = 353
      Height = 21
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 79
    Width = 448
    Height = 193
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Colors '
    TabOrder = 1
    object Label2: TLabel
      Left = 5
      Top = 48
      Width = 48
      Height = 13
      Caption = 'Conflicted'
      FocusControl = cboxConflicted
    end
    object Label3: TLabel
      Left = 5
      Top = 76
      Width = 31
      Height = 13
      Caption = 'Added'
      FocusControl = cboxAdded
    end
    object Label4: TLabel
      Left = 5
      Top = 104
      Width = 135
      Height = 13
      Caption = 'Missing / Deleted / Replaced'
      FocusControl = cboxDeleted
    end
    object Label5: TLabel
      Left = 5
      Top = 132
      Width = 36
      Height = 13
      Caption = 'Merged'
      FocusControl = cboxMerged
    end
    object Label6: TLabel
      Left = 5
      Top = 160
      Width = 40
      Height = 13
      Caption = 'Modified'
      FocusControl = cboxModified
    end
    object cboxConflicted: TColorBox
      Left = 149
      Top = 45
      Width = 145
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
      TabOrder = 1
    end
    object cboxAdded: TColorBox
      Left = 149
      Top = 73
      Width = 145
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
      TabOrder = 2
    end
    object cboxDeleted: TColorBox
      Left = 149
      Top = 101
      Width = 145
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
      TabOrder = 3
    end
    object cboxMerged: TColorBox
      Left = 149
      Top = 129
      Width = 145
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
      TabOrder = 4
    end
    object cboxModified: TColorBox
      Left = 149
      Top = 157
      Width = 145
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
      TabOrder = 5
    end
    object cbStatusColorsEnabled: TCheckBox
      Left = 5
      Top = 22
      Width = 124
      Height = 17
      Caption = 'Enable Colors'
      TabOrder = 0
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 278
    Width = 448
    Height = 235
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Options '
    TabOrder = 2
    object lbAuthentication: TLabel
      Left = 8
      Top = 125
      Width = 141
      Height = 13
      Caption = 'Remote authentication data: '
    end
    object lbUserName: TLabel
      Left = 8
      Top = 180
      Width = 56
      Height = 13
      Caption = 'User Name:'
    end
    object lbEmail: TLabel
      Left = 8
      Top = 207
      Width = 28
      Height = 13
      Caption = 'Email:'
    end
    object Label7: TLabel
      Left = 8
      Top = 155
      Width = 118
      Height = 13
      Caption = 'User identification data: '
    end
    object cbDeleteBackupFilesAfterCommit: TCheckBox
      Left = 5
      Top = 23
      Width = 356
      Height = 17
      Caption = 'Delete backup files after commit'
      TabOrder = 0
    end
    object cbAlternativeCommitLayout: TCheckBox
      Left = 5
      Top = 46
      Width = 356
      Height = 17
      Caption = 'Alternative Commit Layout'
      TabOrder = 1
    end
    object cbClearFileStatesAfterCloseAll: TCheckBox
      Left = 5
      Top = 70
      Width = 356
      Height = 17
      Caption = 'Clear file states after Close All'
      TabOrder = 2
    end
    object cbKeepCommitViewOpenAfterCommit: TCheckBox
      Left = 5
      Top = 94
      Width = 356
      Height = 17
      Caption = 'Keep Commit View open after commit'
      TabOrder = 3
    end
    object btnClear: TButton
      Left = 230
      Top = 117
      Width = 75
      Height = 25
      Caption = 'Clear...'
      TabOrder = 4
      OnClick = btnClearClick
    end
    object btnClearAll: TButton
      Left = 311
      Top = 117
      Width = 75
      Height = 25
      Caption = 'Clear All'
      TabOrder = 5
      OnClick = btnClearAllClick
    end
    object edUserName: TEdit
      Left = 178
      Top = 172
      Width = 208
      Height = 21
      TabOrder = 6
    end
    object edUserEmail: TEdit
      Left = 178
      Top = 199
      Width = 208
      Height = 21
      TabOrder = 7
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = '*.exe|*.exe'
    Left = 8
    Top = 80
  end
end
