inherited SubViewDesignerFrame: TSubViewDesignerFrame
  Width = 497
  Height = 318
  HelpKeyword = 'View'
  ExplicitWidth = 497
  ExplicitHeight = 318
  inherited ClientPanel: TPanel
    Width = 497
    Height = 318
    ExplicitWidth = 497
    ExplicitHeight = 318
    inherited DesignPanel: TPanel
      Width = 497
      Height = 318
      ExplicitWidth = 497
      ExplicitHeight = 318
      object VCPageControl: TPageControl
        Left = 0
        Top = 0
        Width = 497
        Height = 318
        ActivePage = ViewTabSheet
        Align = alClient
        TabOrder = 0
        OnChange = VCPageControlChange
        object ViewTabSheet: TTabSheet
          Caption = 'View'
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
            Caption = 'View'
            TabOrder = 0
            object ImageNameSpeedButton: TSpeedButton
              Left = 370
              Top = 70
              Width = 24
              Height = 23
              Action = FileOpenAction
            end
            object ImageNameImage: TImage
              Left = 399
              Top = 76
              Width = 16
              Height = 16
              Hint = 'Image icon'
              Proportional = True
              Stretch = True
            end
            object ImageNameImageLarge: TImage
              Left = 423
              Top = 60
              Width = 32
              Height = 32
              Hint = 'Image icon large'
              Proportional = True
              Stretch = True
            end
            object ControllerLabel: TLabel
              Left = 6
              Top = 15
              Width = 47
              Height = 13
              Caption = 'Controller'
            end
            object ViewNameSpeedButton: TSpeedButton
              Left = 370
              Top = 30
              Width = 24
              Height = 23
              Action = FileOpenViewAction
            end
            object ViewNameImage: TImage
              Left = 399
              Top = 36
              Width = 16
              Height = 16
              Hint = 'Image icon'
              Proportional = True
              Stretch = True
            end
            object ViewNameImageLarge: TImage
              Left = 423
              Top = 20
              Width = 32
              Height = 32
              Hint = 'Image icon large'
              Proportional = True
              Stretch = True
            end
            object _DisplayLabel: TLabeledEdit
              Left = 6
              Top = 71
              Width = 222
              Height = 21
              EditLabel.Width = 59
              EditLabel.Height = 13
              EditLabel.Caption = 'DisplayLabel'
              TabOrder = 3
            end
            object ImageNameEdit: TLabeledEdit
              Left = 234
              Top = 71
              Width = 136
              Height = 21
              EditLabel.Width = 57
              EditLabel.Height = 13
              EditLabel.Caption = 'ImageName'
              TabOrder = 0
              OnChange = ImageNameEditChange
            end
            object ControllerComboBox: TComboBox
              Left = 6
              Top = 31
              Width = 222
              Height = 21
              TabOrder = 1
              OnChange = ControllerComboBoxChange
            end
            object ViewNameEdit: TLabeledEdit
              Left = 234
              Top = 31
              Width = 136
              Height = 21
              EditLabel.Width = 120
              EditLabel.Height = 13
              EditLabel.Caption = 'View Name (external file)'
              TabOrder = 2
              OnChange = ViewNameEditChange
            end
          end
        end
        object ControllerTabSheet: TTabSheet
          Caption = 'Controller'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
        object MobileSettingsTabSheet: TTabSheet
          Caption = 'MobileSettings'
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
    object FileOpenViewAction: TFileOpen
      Category = 'File'
      Dialog.DefaultExt = '*.yaml'
      Dialog.Filter = 'Kitto View files|*.yaml'
      Hint = 'Open|Opens an existing View file'
      ImageIndex = 0
      ShortCut = 16463
      BeforeExecute = FileOpenViewActionBeforeExecute
      OnAccept = FileOpenViewActionAccept
    end
  end
end
