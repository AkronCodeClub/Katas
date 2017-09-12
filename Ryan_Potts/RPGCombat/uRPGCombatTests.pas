unit uRPGCombatTests;

interface
uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TRPGCombatTests = class(TObject)
  public
    [Test]
    procedure NewCharacterHasCorrectInitialState;

    [Test]
    procedure CharacterCanReceiveDamage;

    [Test]
    procedure DamageExceedsHealthCharacterDies;

    [Test]
    procedure HealInjuredCharacter;

    [Test]
    procedure OnlyHealLivingCharacters;

    [Test]
    procedure CannotOverheal;

    [Test]
    procedure DoubleDamageBecauseAttackerFiveLevelsAbove;

    [Test]
    procedure HalveDamageBecauseAttackerFiveLevelsBelow;
  end;

implementation
uses uRPGCharacter;

{ TRPGCombatTests }

procedure TRPGCombatTests.CannotOverheal;
var
  lRPGCharacterA: IRPGCharacter;
  lRPGCharacterB: IRPGCharacter;
begin
  lRPGCharacterA := TRPGCharacter.Create('Moose');
  lRPGCharacterB := TRPGCharacter.Create('Squirrel');
  lRPGCharacterA.Target := lRPGCharacterB;
  lRPGCharacterA.Damage := 500;
  lRPGCharacterB.Heal := 2000;
  Assert.AreEqual(1000, lRPGCharacterB.Health);
end;

procedure TRPGCombatTests.CharacterCanReceiveDamage;
var
  lRPGCharacterA: IRPGCharacter;
  lRPGCharacterB: IRPGCharacter;
begin
  lRPGCharacterA := TRPGCharacter.Create('Moose');
  lRPGCharacterB := TRPGCharacter.Create('Squirrel');
  lRPGCharacterA.Target := lRPGCharacterB;
  lRPGCharacterA.Damage := 20;
  Assert.AreEqual(1000 - 20, lRPGCharacterB.Health);
end;

procedure TRPGCombatTests.DamageExceedsHealthCharacterDies;
var
  lRPGCharacterA: IRPGCharacter;
  lRPGCharacterB: IRPGCharacter;
begin
  lRPGCharacterA := TRPGCharacter.Create('Moose');
  lRPGCharacterB := TRPGCharacter.Create('Squirrel');
  lRPGCharacterA.Target := lRPGCharacterB;
  lRPGCharacterA.Damage := 2000;
  Assert.AreEqual(0, lRPGCharacterB.Health);
  Assert.AreEqual(False, lRPGCharacterB.Alive);
end;

procedure TRPGCombatTests.DoubleDamageBecauseAttackerFiveLevelsAbove;
var
  lRPGCharacterA: IRPGCharacter;
  lRPGCharacterB: IRPGCharacter;
begin
  lRPGCharacterA := TRPGCharacter.Create('Moose');
  lRPGCharacterB := TRPGCharacter.Create('Squirrel');
  lRPGCharacterA.Level := 6;
  lRPGCharacterA.Target := lRPGCharacterB;
  lRPGCharacterA.Damage := 100;
  Assert.AreEqual(1000 - 200, lRPGCharacterB.Health);
end;

procedure TRPGCombatTests.HalveDamageBecauseAttackerFiveLevelsBelow;
var
  lRPGCharacterA: IRPGCharacter;
  lRPGCharacterB: IRPGCharacter;
begin
  lRPGCharacterA := TRPGCharacter.Create('Moose');
  lRPGCharacterB := TRPGCharacter.Create('Squirrel');
  lRPGCharacterB.Level := 6;
  lRPGCharacterA.Target := lRPGCharacterB;
  lRPGCharacterA.Damage := 100;
  Assert.AreEqual(1000 - 50, lRPGCharacterB.Health);
end;

procedure TRPGCombatTests.HealInjuredCharacter;
var
  lRPGCharacterA: IRPGCharacter;
  lRPGCharacterB: IRPGCharacter;
begin
  lRPGCharacterA := TRPGCharacter.Create('Moose');
  lRPGCharacterB := TRPGCharacter.Create('Squirrel');
  lRPGCharacterA.Target := lRPGCharacterB;
  lRPGCharacterA.Damage := 500;
  lRPGCharacterB.Heal := 250;
  Assert.AreEqual(750, lRPGCharacterB.Health);
end;

procedure TRPGCombatTests.NewCharacterHasCorrectInitialState;
var
  lRPGCharacter: IRPGCharacter;
begin
  lRPGCharacter := TRPGCharacter.Create;
  Assert.AreEqual(1000,lRPGCharacter.Health);
  Assert.AreEqual(1,lRPGCharacter.Level);
  Assert.AreEqual(True,lRPGCharacter.Alive);
end;

procedure TRPGCombatTests.OnlyHealLivingCharacters;
var
  lRPGCharacterA: IRPGCharacter;
  lRPGCharacterB: IRPGCharacter;
begin
  lRPGCharacterA := TRPGCharacter.Create('Moose');
  lRPGCharacterB := TRPGCharacter.Create('Squirrel');
  lRPGCharacterA.Target := lRPGCharacterB;
  lRPGCharacterA.Damage := 2000;
  lRPGCharacterB.Heal := 250;
  Assert.AreEqual(False, lRPGCharacterB.Alive);
end;

initialization
  TDUnitX.RegisterTestFixture(TRPGCombatTests);
end.
