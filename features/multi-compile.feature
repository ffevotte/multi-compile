Feature:

  Scenario: Run compilation using execute-command
    Given Buffer "*build*" does not exist
    When I start an action chain
    And    I press "M-x"
    And    I type "build"
    And  I execute the action chain
    And  I wait for 0.1 seconds
    Then I should see pattern "^build" in buffer "*build*"

    Given Buffer "*build*" is empty
    And  I start an action chain
    And    I press "M-x"
    And    I type "rebuild"
    And  I execute the action chain
    And  I switch to buffer "*build*"
    And  I wait for 0.1 seconds
    Then I should see pattern "^build" in buffer "*build*"

  Scenario: Run compilation using the key binding
    Given Buffer "*build*" does not exist
    When I press "<f5>"
    And  I wait for 0.1 seconds
    Then I should see pattern "^build" in buffer "*build*"

    Given Buffer "*build*" is empty
    And  I press "<f5>"
    And  I wait for 0.1 seconds
    Then I should see pattern "^build" in buffer "*build*"

    Given Buffer "*build*" is empty
    And  I press "C-u <f5> r"
    And  I wait for 0.1 seconds
    Then I should see pattern "^build" in buffer "*build*"

    Given Buffer "*build*" is empty
    When I start an action chain
    And    I press "C-u <f5> c"
    And    I press "C-S-<backspace>"
    And    I type "echo new command"
    And  I execute the action chain
    And  I wait for 0.1 seconds
    Then I should see pattern "^new command" in buffer "*build*"

    Given Buffer "*build*" is empty
    When I start an action chain
    And    I press "C-u <f5> d"
    And    I press "C-S-<backspace>"
    And    I type "/tmp"
    And  I execute the action chain
    And  I wait for 0.1 seconds
    Then I should see pattern "default-directory: \"/tmp/\"" in buffer "*build*"

    Given Buffer "*build*" is empty
    When I press "C-u <f5> R"
    And  I wait for 0.1 seconds
    Then I should see pattern "^build" in buffer "*build*"
    But  I should not see pattern "default-directory: \"/tmp/\"" in buffer "*build*"


  Scenario: Compilation with specified default command
    Given Buffer "*check*" does not exist
    When I press "<f6>"
    And  I wait for 0.1 seconds
    Then I should see pattern "^check" in buffer "*check*"
