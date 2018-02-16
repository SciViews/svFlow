context("flow")

describe("flow", {
  fl <- flow()

  it("produces a Correlation, matrix object", {
    expect_is(fl, "flow")
  })
  rm(fl)
})
