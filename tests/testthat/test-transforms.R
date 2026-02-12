describe("check_wb_command", {
  it("returns provided path if it exists", {
    skip_if_not_installed("ciftiTools")
    tmp <- withr::local_tempfile()
    file.create(tmp)
    expect_equal(check_wb_command(tmp), tmp)
  })

  it("errors for non-existent explicit path", {
    skip_if_not_installed("ciftiTools")
    expect_error(
      check_wb_command("/nonexistent/wb_command"),
      "not found"
    )
  })

  it("errors when wb_command not available anywhere", {
    skip_if_not_installed("ciftiTools")

    local_mocked_bindings(
      ciftiTools.getOption = function(...) NULL,
      .package = "ciftiTools"
    )
    withr::local_envvar(PATH = "")

    expect_error(check_wb_command(), "wb_command")
  })
})

describe("transform_to_space", {
  it("errors for missing ciftiTools", {
    skip_if(rlang::is_installed("ciftiTools"))
    expect_error(
      transform_to_space("test.gii"),
      "ciftiTools"
    )
  })

  it("errors for non-existent file", {
    skip_if_not_installed("ciftiTools")

    local_mocked_bindings(
      check_wb_command = function(...) "/mock/wb_command"
    )
    local_mocked_bindings(
      ciftiTools.setOption = function(...) NULL,
      .package = "ciftiTools"
    )

    expect_error(
      transform_to_space("/nonexistent/file.gii", verbose = FALSE),
      "not found"
    )
  })
})
