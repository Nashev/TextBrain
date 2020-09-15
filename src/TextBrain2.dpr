program TextBrain2;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm2Unit in 'MainForm2Unit.pas' {MainForm},
  Engine2.Interfaces in 'Engine2.Interfaces.pas',
  Spring in '..\spring4d\Source\Base\Spring.pas',
  Spring.Events in '..\spring4d\Source\Base\Spring.Events.pas',
  Spring.Events.Base in '..\spring4d\Source\Base\Spring.Events.Base.pas',
  Spring.ResourceStrings in '..\spring4d\Source\Base\Spring.ResourceStrings.pas',
  Spring.ValueConverters in '..\spring4d\Source\Base\Spring.ValueConverters.pas',
  Spring.SystemUtils in '..\spring4d\Source\Base\Spring.SystemUtils.pas',
  Spring.VirtualClass in '..\spring4d\Source\Base\Spring.VirtualClass.pas',
  Spring.Collections.Adapters in '..\spring4d\Source\Base\Collections\Spring.Collections.Adapters.pas',
  Spring.Collections.Base in '..\spring4d\Source\Base\Collections\Spring.Collections.Base.pas',
  Spring.Collections.Dictionaries in '..\spring4d\Source\Base\Collections\Spring.Collections.Dictionaries.pas',
  Spring.Collections.Enumerable in '..\spring4d\Source\Base\Collections\Spring.Collections.Enumerable.pas',
  Spring.Collections.Events in '..\spring4d\Source\Base\Collections\Spring.Collections.Events.pas',
  Spring.Collections.Extensions in '..\spring4d\Source\Base\Collections\Spring.Collections.Extensions.pas',
  Spring.Collections.LinkedLists in '..\spring4d\Source\Base\Collections\Spring.Collections.LinkedLists.pas',
  Spring.Collections.Lists in '..\spring4d\Source\Base\Collections\Spring.Collections.Lists.pas',
  Spring.Collections.MultiMaps in '..\spring4d\Source\Base\Collections\Spring.Collections.MultiMaps.pas',
  Spring.Collections in '..\spring4d\Source\Base\Collections\Spring.Collections.pas',
  Spring.Collections.Queues in '..\spring4d\Source\Base\Collections\Spring.Collections.Queues.pas',
  Spring.Collections.Sets in '..\spring4d\Source\Base\Collections\Spring.Collections.Sets.pas',
  Spring.Collections.Stacks in '..\spring4d\Source\Base\Collections\Spring.Collections.Stacks.pas',
  Spring.Collections.Trees in '..\spring4d\Source\Base\Collections\Spring.Collections.Trees.pas',
  Engine2Unit in 'Engine2Unit.pas',
  Spring.Services in '..\spring4d\Source\Core\Services\Spring.Services.pas',
  Spring.Container.Common in '..\spring4d\Source\Core\Container\Spring.Container.Common.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
