inherited ActionToolDesignerFrame: TActionToolDesignerFrame
  Width = 497
  Height = 406
  HelpKeyword = 'HowToAction'
  ExplicitWidth = 497
  ExplicitHeight = 406
  inherited ClientPanel: TPanel
    Width = 497
    Height = 406
    ExplicitWidth = 497
    ExplicitHeight = 406
    inherited DesignPanel: TPanel
      Width = 497
      Height = 406
      ExplicitWidth = 497
      ExplicitHeight = 406
      object VCPageControl: TPageControl
        Left = 0
        Top = 0
        Width = 497
        Height = 406
        ActivePage = ViewTabSheet
        Align = alClient
        TabOrder = 0
        OnChange = VCPageControlChange
        object ViewTabSheet: TTabSheet
          Caption = 'ActionTool'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object ViewGroupBox: TGroupBox
            Left = 0
            Top = 0
            Width = 489
            Height = 99
            Align = alTop
            Caption = 'ActionButton'
            TabOrder = 0
            object ImageNameSpeedButton: TSpeedButton
              Left = 370
              Top = 29
              Width = 24
              Height = 23
              Action = FileOpenAction
            end
            object ImageNameImageLarge: TImage
              Left = 423
              Top = 19
              Width = 32
              Height = 32
              Hint = 'Image icon large'
              Proportional = True
              Stretch = True
            end
            object ImageNameImage: TImage
              Left = 399
              Top = 35
              Width = 16
              Height = 16
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
              EditLabel.Width = 57
              EditLabel.Height = 13
              EditLabel.Caption = 'ImageName'
              TabOrder = 1
              OnChange = ImageNameEditChange
            end
            object _Controller: TLabeledEdit
              Left = 88
              Top = 70
              Width = 306
              Height = 21
              EditLabel.Width = 47
              EditLabel.Height = 13
              EditLabel.Caption = 'Controller'
              TabOrder = 3
            end
            object _IsVisible: TCheckBox
              Left = 6
              Top = 70
              Width = 76
              Height = 17
              Caption = 'IsVisible'
              TabOrder = 2
            end
          end
        end
        object ControllerTabsheet: TTabSheet
          Caption = 'Controller'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
        object BeforeExecuteTabSheet: TTabSheet
          Caption = 'BeforeExecute'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
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
