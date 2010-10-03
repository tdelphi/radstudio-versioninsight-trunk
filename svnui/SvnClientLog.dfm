object SvnLogFrame: TSvnLogFrame
  Left = 0
  Top = 0
  Width = 886
  Height = 731
  HelpContext = 15204
  DoubleBuffered = True
  ParentDoubleBuffered = False
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 0
    Top = 410
    Width = 886
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 0
    ExplicitWidth = 400
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 588
    Width = 886
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitLeft = 188
    ExplicitTop = 0
    ExplicitWidth = 360
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 886
    Height = 410
    Align = alClient
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 0
    object Revisions: TListView
      Left = 0
      Top = 23
      Width = 886
      Height = 387
      Align = alClient
      Columns = <
        item
          Caption = 'Revision'
          Width = -2
          WidthType = (
            -2)
        end
        item
          Caption = 'Author'
          Width = -2
          WidthType = (
            -2)
        end
        item
          Caption = 'Date'
          Width = -2
          WidthType = (
            -2)
        end
        item
          Caption = 'Comment'
          Width = -2
          WidthType = (
            -2)
        end>
      OwnerData = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnData = RevisionsData
      OnSelectItem = RevisionsSelectItem
    end
    object ToolBar1: TToolBar
      Left = 0
      Top = 0
      Width = 886
      Height = 23
      Caption = 'ToolBar1'
      Images = ImageList
      TabOrder = 1
      object Refresh: TToolButton
        Left = 0
        Top = 0
        Hint = 'Refresh'
        Caption = 'Refresh'
        Enabled = False
        ImageIndex = 2
        OnClick = RefreshClick
      end
      object Next: TToolButton
        Left = 23
        Top = 0
        Hint = 'Next 100'
        Caption = 'Next 100'
        Enabled = False
        ImageIndex = 1
        OnClick = NextClick
      end
      object Search: TButtonedEdit
        Left = 46
        Top = 0
        Width = 325
        Height = 22
        Images = ImageList
        LeftButton.HotImageIndex = 0
        LeftButton.ImageIndex = 0
        LeftButton.PressedImageIndex = 0
        LeftButton.Visible = True
        RightButton.HotImageIndex = 4
        RightButton.ImageIndex = 3
        RightButton.PressedImageIndex = 5
        TabOrder = 0
        TextHint = 'Search'
        OnKeyDown = SearchKeyDown
        OnKeyPress = SearchKeyPress
        OnKeyUp = SearchKeyUp
        OnRightButtonClick = SearchRightButtonClick
      end
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 591
    Width = 886
    Height = 140
    Align = alBottom
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 2
    object Files: TListView
      Left = 0
      Top = 0
      Width = 886
      Height = 140
      Align = alClient
      Columns = <
        item
          Caption = 'Action'
          Width = -2
          WidthType = (
            -2)
        end
        item
          Caption = 'File'
          Width = -2
          WidthType = (
            -2)
        end>
      OwnerData = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnData = FilesData
    end
  end
  object CenterPanel: TPanel
    Left = 0
    Top = 413
    Width = 886
    Height = 175
    Align = alBottom
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 1
    object Comment: TMemo
      Left = 0
      Top = 0
      Width = 886
      Height = 175
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object ImageList: TImageList
    Left = 765
    Top = 2
    Bitmap = {
      494C010103000800640010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CC33330000000000000000000000000000000000A4A0A000404040004040
      4000404040004040400040404000404040004040400040404000404040004040
      400040404000A4A0A00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000427B8400427B840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000980000000000000000000000000000004040400080E0E00080E0
      E00080E0E00080E0E00080E0E00080E0E00080E0E00080E0E00080E0E00080E0
      E00080E0E0004040400000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000427B
      8400427B8400427B840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000098000000000000000000000040404000F0FBFF00F0FB
      FF00F0FBFF00F0FBFF0080208000F0FBFF00F0FBFF00F0FBFF00F0FBFF00F0FB
      FF0080E0E0004040400000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000427B8400427B
      8400427B84000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CC333300CC333300CC333300CC33
      3300CC333300CC333300CC333300980000000000000040404000F0FBFF00F0FB
      FF00F0FBFF008020800080208000F0FBFF00F0FBFF00F0FBFF00F0FBFF00F0FB
      FF0080E0E0004040400000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000427B8400427B8400427B8400427B8400427B8400427B8400427B8400427B
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000098000000000000000000000040404000F0FBFF00F0FB
      FF008060A00080208000802080008020800080208000C0A0C000F0FBFF00F0FB
      FF0080E0E0004040400000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000427B
      8400427B8400000000000000000000000000427B8400427B8400427B84000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009800000000000000000000000000000040404000F0FBFF00F0FB
      FF00F0FBFF008060A00080208000F0FBFF00F0FBFF0080208000F0FBFF00F0FB
      FF0080E0E0004040400000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000427B8400427B
      84000000000000000000000000000000000000000000427B8400427B84000000
      000000000000000000000000000000000000999999004D4D4D004D4D4D004D4D
      4D004D4D4D0053535300999999000000000000000000999999004D4D4D009999
      9900CC333300999999004D4D4D00999999000000000040404000F0FBFF00F0FB
      FF00F0FBFF00F0FBFF008060A000F0FBFF00F0FBFF0080208000F0FBFF00F0FB
      FF0080E0E0004040400000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000427B84000000
      0000000000000000000000000000000000000000000000000000427B84000000
      00000000000000000000000000000000000066666600DFFBFD00DFFBFD00DFFB
      FD00DFFBFD00DFFBFD004D4D4D00000000000000000066666600DFFBFD00DFFB
      FD00DFFBFD00DFFBFD00DFFBFD004D4D4D000000000040404000F0FBFF00F0FB
      FF0080208000F0FBFF00F0FBFF00F0FBFF00F0FBFF0080208000F0FBFF00F0FB
      FF0080E0E0004040400000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000427B84000000
      0000000000000000000000000000000000000000000000000000427B84000000
      00000000000000000000000000000000000066666600A7F4F600535353005353
      530053535300DFFBFD004D4D4D00000000000000000066666600A7F4F6005353
      53005353530053535300DFFBFD004D4D4D000000000040404000F0FBFF00F0FB
      FF0080208000F0FBFF00F0FBFF0080208000F0FBFF00F0FBFF00F0FBFF00F0FB
      FF0080E0E0004040400000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000427B84000000
      0000000000000000000000000000000000000000000000000000427B84000000
      00000000000000000000000000000000000066666600DFFBFD00DFFBFD00DFFB
      FD00DFFBFD00DFFBFD004D4D4D00000000000000000066666600DFFBFD00DFFB
      FD00DFFBFD00DFFBFD00DFFBFD004D4D4D000000000040404000F0FBFF00F0FB
      FF0080208000F0FBFF00F0FBFF008020800080208000F0FBFF00F0FBFF00F0FB
      FF0080E0E0004040400000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000427B8400427B
      84000000000000000000000000000000000000000000427B8400427B84000000
      00000000000000000000000000000000000066666600A7F4F600535353005353
      530053535300DFFBFD004D4D4D00000000000000000066666600A7F4F6005353
      53005353530053535300DFFBFD004D4D4D000000000040404000F0FBFF00F0FB
      FF00C0A0C000802080008020800080208000802080008060A000F0FBFF0080E0
      E00080E0E0004040400000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000427B
      8400427B8400000000000000000000000000427B8400427B8400000000000000
      00000000000000000000000000000000000066666600DFFBFD00DFFBFD00DFFB
      FD00DFFBFD0099CCCC004D4D4D00000000000000000066666600DFFBFD00DFFB
      FD00DFFBFD00DFFBFD0099CCCC004D4D4D000000000040404000F0FBFF00F0FB
      FF00F0FBFF00F0FBFF00F0FBFF00802080008060A000F0FBFF00406060004060
      6000406060004040400000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000427B8400427B8400427B8400427B8400427B840000000000000000000000
      00000000000000000000000000000000000066666600A7F4F60053535300DFFB
      FD00999999004D4D4D004D4D4D00000000000000000066666600A7F4F6005353
      5300DFFBFD00999999004D4D4D004D4D4D000000000040404000F0FBFF00F0FB
      FF00F0FBFF00F0FBFF00F0FBFF008060A000F0FBFF00F0FBFF0040606000F0FB
      FF00F0FBFF004040400000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000066666600DFFBFD00DFFBFD00DFFB
      FD0066666600A1F3F9004D4D4D00000000000000000066666600DFFBFD00DFFB
      FD00DFFBFD0066666600A1F3F900535353000000000040404000F0FBFF00F0FB
      FF00F0FBFF00F0FBFF00F0FBFF00F0FBFF00F0FBFF00F0FBFF0040606000F0FB
      FF00404040000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009999990066666600666666006666
      6600666666006666660000000000000000000000000099999900666666006666
      66006666660066666600666666000000000000000000A4A0A000404040004040
      4000404040004040400040404000404040004040400040404000404040004040
      4000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF0000FFFFFFF780030000
      FFF3FFFB80030000FFE3FFFD80030000FFC7FF0080030000F00FFFFD80030000
      E71FFFFB80030000CF9F018080030000DFDF018080030000DFDF018080030000
      DFDF018080030000CF9F018080030000E73F018080030000F07F018080030000
      FFFF018080070000FFFF0381800F000000000000000000000000000000000000
      000000000000}
  end
end
