inherited ListControllerFilterApplyButtonFrame: TListControllerFilterApplyButtonFrame
  Width = 333
  Height = 223
  ExplicitWidth = 333
  ExplicitHeight = 223
  inherited ClientPanel: TPanel
    Width = 333
    Height = 223
    ExplicitWidth = 333
    ExplicitHeight = 223
    inherited DesignPanel: TPanel
      Width = 333
      Height = 223
      Padding.Top = 50
      ExplicitWidth = 333
      ExplicitHeight = 223
      object ImageNameSpeedButton: TSpeedButton
        Left = 144
        Top = 56
        Width = 24
        Height = 23
        Action = FileOpenAction
      end
      object ImageNameImage: TImage
        Left = 173
        Top = 62
        Width = 16
        Height = 16
        Hint = 'Image icon'
        Proportional = True
        Stretch = True
      end
      object ImageNameImageLarge: TImage
        Left = 197
        Top = 46
        Width = 32
        Height = 32
        Hint = 'Image icon large'
        Proportional = True
        Stretch = True
      end
      object ImageNameEdit: TLabeledEdit
        Left = 8
        Top = 57
        Width = 136
        Height = 21
        EditLabel.Width = 57
        EditLabel.Height = 13
        EditLabel.Caption = 'ImageName'
        TabOrder = 0
        OnChange = ImageNameEditChange
      end
      object LabelEdit: TLabeledEdit
        Left = 8
        Top = 18
        Width = 221
        Height = 21
        EditLabel.Width = 25
        EditLabel.Height = 13
        EditLabel.Caption = 'Label'
        TabOrder = 1
      end
    end
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
