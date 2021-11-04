Feature: By default, both scenarios pass
  The one tagged @shaky does so by default
  But if `eldev--ecukes-pass-if' is changed, then it fails

  @ok
  Scenario: Always pass
    And ignore value "abc"
    But this always passes

  @shaky
  Scenario: Maybe pass, maybe not
    When `eldev--ecukes-pass-if' is "foo", then pass
    And ignore value "def"
