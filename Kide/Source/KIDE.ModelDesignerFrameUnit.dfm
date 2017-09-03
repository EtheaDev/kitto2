inherited ModelDesignerFrame: TModelDesignerFrame
  Width = 445
  Height = 461
  HelpKeyword = 'Models'
  ExplicitWidth = 445
  ExplicitHeight = 461
  inherited ClientPanel: TPanel
    Width = 424
    Height = 461
    ExplicitWidth = 378
    ExplicitHeight = 429
    inherited DesignPanel: TPanel
      Width = 424
      Height = 404
      ExplicitWidth = 378
      ExplicitHeight = 372
      inherited ToolBar: TToolBar
        Width = 424
        ExplicitWidth = 378
      end
      object EditorPageControl: TPageControl
        Left = 0
        Top = 22
        Width = 424
        Height = 382
        ActivePage = ModelTabSheet
        Align = alClient
        TabOrder = 1
        OnChange = EditorPageControlChange
        ExplicitWidth = 378
        ExplicitHeight = 350
        object ModelTabSheet: TTabSheet
          Caption = 'Model'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 370
          ExplicitHeight = 322
          object ModelScrollBox: TScrollBox
            Left = 0
            Top = 0
            Width = 416
            Height = 354
            Align = alClient
            BevelInner = bvNone
            BevelOuter = bvNone
            BorderStyle = bsNone
            TabOrder = 0
            ExplicitWidth = 370
            ExplicitHeight = 322
            object ApplicationGroupBox: TGroupBox
              Left = 0
              Top = 0
              Width = 416
              Height = 325
              Align = alTop
              Caption = 'Model'
              TabOrder = 0
              ExplicitWidth = 367
              DesignSize = (
                416
                325)
              object ImageNameSpeedButton: TSpeedButton
                Left = 303
                Top = 29
                Width = 24
                Height = 23
                Action = FileOpenAction
              end
              object ImageNameImage: TImage
                Left = 329
                Top = 19
                Width = 32
                Height = 32
                Hint = 'Image icon'
                Proportional = True
                Stretch = True
              end
              object _ModelName: TLabeledEdit
                Left = 5
                Top = 30
                Width = 156
                Height = 21
                EditLabel.Width = 55
                EditLabel.Height = 13
                EditLabel.Caption = 'ModelName'
                TabOrder = 0
              end
              object _DisplayLabel: TLabeledEdit
                Left = 5
                Top = 110
                Width = 156
                Height = 21
                EditLabel.Width = 59
                EditLabel.Height = 13
                EditLabel.Caption = 'DisplayLabel'
                TabOrder = 4
              end
              object _PluralDisplayLabel: TLabeledEdit
                Left = 167
                Top = 110
                Width = 244
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                EditLabel.Width = 85
                EditLabel.Height = 13
                EditLabel.Caption = 'PluralDisplayLabel'
                TabOrder = 5
                ExplicitWidth = 195
              end
              object _PhysicalName: TLabeledEdit
                Left = 5
                Top = 69
                Width = 156
                Height = 21
                EditLabel.Width = 65
                EditLabel.Height = 13
                EditLabel.Caption = 'PhysicalName'
                TabOrder = 2
              end
              object _CaptionField: TLabeledEdit
                Left = 5
                Top = 177
                Width = 156
                Height = 21
                EditLabel.Width = 59
                EditLabel.Height = 13
                EditLabel.Caption = 'CaptionField'
                TabOrder = 8
              end
              object _DatabaseRouter: TLabeledEdit
                Left = 5
                Top = 299
                Width = 156
                Height = 21
                EditLabel.Width = 79
                EditLabel.Height = 13
                EditLabel.Caption = 'DatabaseRouter'
                TabOrder = 11
              end
              object _IsLarge: TCheckBox
                Left = 5
                Top = 137
                Width = 97
                Height = 17
                Caption = 'IsLarge'
                TabOrder = 6
              end
              object _IsReadOnly: TCheckBox
                Left = 167
                Top = 137
                Width = 97
                Height = 17
                Caption = 'IsReadOnly'
                TabOrder = 7
              end
              object _DefaultSorting: TLabeledEdit
                Left = 5
                Top = 220
                Width = 406
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                EditLabel.Width = 69
                EditLabel.Height = 13
                EditLabel.Caption = 'DefaultSorting'
                TabOrder = 9
                ExplicitWidth = 357
              end
              object _ImageName: TLabeledEdit
                Left = 167
                Top = 30
                Width = 136
                Height = 21
                EditLabel.Width = 57
                EditLabel.Height = 13
                EditLabel.Caption = 'ImageName'
                TabOrder = 1
                OnChange = _ImageNameChange
              end
              object _PluralModelName: TLabeledEdit
                Left = 167
                Top = 69
                Width = 244
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                EditLabel.Width = 81
                EditLabel.Height = 13
                EditLabel.Caption = 'PluralModelName'
                TabOrder = 3
                ExplicitWidth = 195
              end
              object _DefaultFilter: TLabeledEdit
                Left = 5
                Top = 260
                Width = 406
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                EditLabel.Width = 59
                EditLabel.Height = 13
                EditLabel.Caption = 'DefaultFilter'
                TabOrder = 10
                ExplicitWidth = 357
              end
              object _DatabaseRouter_DatabaseName: TLabeledEdit
                Left = 167
                Top = 299
                Width = 244
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                EditLabel.Width = 156
                EditLabel.Height = 13
                EditLabel.Caption = 'DatabaseRouter/DatabaseName'
                TabOrder = 12
                ExplicitWidth = 195
              end
            end
            object ModelAutoScrollPanel: TPanel
              Left = 0
              Top = 325
              Width = 367
              Height = 29
              Align = alLeft
              BevelOuter = bvNone
              TabOrder = 1
              ExplicitHeight = 0
            end
          end
        end
        object FieldsTabSheet: TTabSheet
          Caption = 'Fields'
          ImageIndex = 3
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
        object DetailReferencesTabSheet: TTabSheet
          Caption = 'DetailReferences'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
        object RulesTabSheet: TTabSheet
          Caption = 'Rules'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 370
          ExplicitHeight = 322
        end
      end
    end
    inherited PathPanel: TStaticText
      Width = 424
    end
    inherited CodeEditorFrame: TCodeEditorFrame
      Top = 421
      Width = 424
      ExplicitTop = 421
      ExplicitWidth = 424
      inherited ErrorLabel: TLabel
        Width = 424
        ExplicitTop = 310
        ExplicitWidth = 386
      end
      inherited ToolBar: TToolBar
        Width = 424
        ExplicitWidth = 378
      end
    end
  end
  inherited TabSet: TTabSet
    Left = 424
    Height = 461
    ExplicitLeft = 378
    ExplicitHeight = 429
  end
  inherited ActionList: TActionList
    Left = 256
    Top = 200
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
