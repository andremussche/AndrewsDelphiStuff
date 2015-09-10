program SchoolFlashViewer;

uses
  Forms,
  controlPosition in 'controlPosition.pas',
  uComponentMover in 'uComponentMover.pas',
  uDragDrop in 'uDragDrop.pas',
  uFormData in 'uFormData.pas',
  cTranspMemo in 'cTranspMemo.pas',
  fTransparentForm in 'fTransparentForm.pas' {frmTransparent},
  fMainWithBmp in 'fMainWithBmp.pas' {frmMainBmp},
  fFlashOffscreen in 'fFlashOffscreen.pas' {frmFlashOffscreen},
  uFlashMemo in 'uFlashMemo.pas',
  uFlashRect in 'uFlashRect.pas',
  uFlashQuestion in 'uFlashQuestion.pas',
  fImagePopup in 'fImagePopup.pas' {frmImagePopup};

{$R *.res}

//done: tekst vak: geen randen indien geen edit mode
//done: nieuw tekstvak: niet juiste tekst grootte + niet grips
//done: met ctrl/shift pijltjes pixels verplaatsen
//redraw after move
//inc/dec font size

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'School Flash Viewer';
  Application.CreateForm(TfrmMainBmp, frmMainBmp);
  Application.CreateForm(TfrmFlashOffscreen, frmFlashOffscreen);
  Application.Run;
end.
