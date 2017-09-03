inherited ViewNodeFrame: TViewNodeFrame
  Width = 466
  Height = 318
  HelpKeyword = 'Views'
  ExplicitWidth = 466
  ExplicitHeight = 318
  inherited ClientPanel: TPanel
    Width = 466
    Height = 318
    ExplicitWidth = 466
    ExplicitHeight = 318
    inherited DesignPanel: TPanel
      Width = 466
      Height = 318
      ExplicitWidth = 466
      ExplicitHeight = 318
      object VCPageControl: TPageControl
        Left = 0
        Top = 0
        Width = 466
        Height = 318
        ActivePage = ViewTabSheet
        Align = alClient
        TabOrder = 0
        object ViewTabSheet: TTabSheet
          Caption = 'View'
          object ViewGroupBox: TGroupBox
            Left = 0
            Top = 0
            Width = 458
            Height = 99
            Align = alTop
            Caption = 'View'
            TabOrder = 0
            object ImageNameSpeedButton: TSpeedButton
              Left = 370
              Top = 29
              Width = 24
              Height = 23
              Action = FileOpenAction
            end
            object ImageNameImage: TImage
              Left = 396
              Top = 19
              Width = 32
              Height = 32
              Hint = 'Image icon'
              Proportional = True
              Stretch = True
            end
            object _DisplayLabel: TLabeledEdit
              Left = 6
              Top = 30
              Width = 222
              Height = 21
              EditLabel.Width = 59
              EditLabel.Height = 13
              EditLabel.Caption = 'DisplayLabel'
              TabOrder = 0
            end
            object ImageNameEdit: TLabeledEdit
              Left = 234
              Top = 30
              Width = 136
              Height = 21
              EditLabel.Width = 75
              EditLabel.Height = 13
              EditLabel.Caption = 'ImageNameEdit'
              TabOrder = 1
              OnChange = ImageNameEditChange
            end
            object _Controller: TLabeledEdit
              Left = 6
              Top = 71
              Width = 222
              Height = 21
              EditLabel.Width = 47
              EditLabel.Height = 13
              EditLabel.Caption = 'Controller'
              TabOrder = 2
            end
          end
        end
        object Controller: TTabSheet
          Caption = 'Controller'
          ImageIndex = 1
        end
      end
    end
  end
  inherited ActionList: TActionList
    Top = 184
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
