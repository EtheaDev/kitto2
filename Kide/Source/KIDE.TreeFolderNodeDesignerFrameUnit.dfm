inherited TreeFolderNodeDesignerFrame: TTreeFolderNodeDesignerFrame
  Width = 354
  HelpKeyword = 'TreeView'
  ExplicitWidth = 354
  inherited ClientPanel: TPanel
    Width = 354
    ExplicitWidth = 354
    inherited DesignPanel: TPanel
      Width = 354
      ExplicitWidth = 354
      object ImageNameSpeedButton: TSpeedButton
        Left = 142
        Top = 61
        Width = 24
        Height = 23
        Action = FileOpenAction
      end
      object ImageNameImage: TImage
        Left = 171
        Top = 67
        Width = 16
        Height = 16
        Hint = 'Image icon'
        Proportional = True
        Stretch = True
      end
      object ImageNameImageLarge: TImage
        Left = 195
        Top = 51
        Width = 32
        Height = 32
        Hint = 'Image icon large'
        Proportional = True
        Stretch = True
      end
      object ValueEdit: TLabeledEdit
        Left = 6
        Top = 18
        Width = 221
        Height = 21
        EditLabel.Width = 58
        EditLabel.Height = 13
        EditLabel.Caption = 'Folder Label'
        TabOrder = 0
      end
      object ImageNameEdit: TLabeledEdit
        Left = 6
        Top = 62
        Width = 136
        Height = 21
        EditLabel.Width = 57
        EditLabel.Height = 13
        EditLabel.Caption = 'ImageName'
        TabOrder = 1
        OnChange = ImageNameEditChange
      end
    end
  end
  object _IsInitiallyCollapsed: TCheckBox [1]
    Left = 6
    Top = 99
    Width = 115
    Height = 17
    Caption = 'IsInitiallyCollapsed'
    TabOrder = 1
  end
  inherited ActionList: TActionList
    object FileOpenAction: TFileOpen
      Category = 'File'
      Dialog.DefaultExt = '*.png'
      Dialog.Filter = 
        'png image files|*.png|gif image files|*.gif|jpeg image files|*.j' +
        'pg;*.jpeg|bitmap image files|*.bmp|All files|*.*'
      Hint = 'Open|Opens an existing file'
      ImageIndex = 0
      ShortCut = 16463
      OnAccept = FileOpenActionAccept
    end
  end
end
