inherited SendEmailMessageNodeFrame: TSendEmailMessageNodeFrame
  Width = 449
  Height = 384
  ExplicitWidth = 449
  ExplicitHeight = 384
  inherited ClientPanel: TPanel
    Width = 449
    Height = 384
    ExplicitWidth = 449
    ExplicitHeight = 384
    inherited DesignPanel: TPanel
      Width = 449
      Height = 384
      ExplicitWidth = 449
      ExplicitHeight = 384
      object EmailPageControl: TPageControl
        Left = 0
        Top = 0
        Width = 449
        Height = 384
        ActivePage = MessageTabSheet
        Align = alClient
        TabOrder = 0
        object MessageTabSheet: TTabSheet
          Caption = 'Email message'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          DesignSize = (
            441
            356)
          object BodyLabel: TLabel
            Left = 3
            Top = 78
            Width = 24
            Height = 13
            Caption = 'Body'
          end
          object _From: TLabeledEdit
            Left = 3
            Top = 16
            Width = 433
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 24
            EditLabel.Height = 13
            EditLabel.Caption = 'From'
            TabOrder = 0
          end
          object _Subject: TLabeledEdit
            Left = 3
            Top = 55
            Width = 433
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 36
            EditLabel.Height = 13
            EditLabel.Caption = 'Subject'
            TabOrder = 1
          end
          object _Body: TMemo
            Left = 3
            Top = 95
            Width = 433
            Height = 258
            Anchors = [akLeft, akTop, akRight, akBottom]
            ScrollBars = ssBoth
            TabOrder = 2
          end
        end
        object DestinationTabSheet: TTabSheet
          Caption = 'Destination'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object PageControl2: TPageControl
            Left = 0
            Top = 0
            Width = 441
            Height = 356
            ActivePage = ToTabSheet
            Align = alClient
            TabOrder = 0
            object ToTabSheet: TTabSheet
              Caption = 'To'
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
            end
            object CCTabSheet: TTabSheet
              Caption = 'CC'
              ImageIndex = 1
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
            end
            object BCCTabSheet: TTabSheet
              Caption = 'BCC'
              ImageIndex = 2
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
            end
          end
        end
        object AttachmentsTabSheet: TTabSheet
          Caption = 'Attachments'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
      end
    end
  end
end
