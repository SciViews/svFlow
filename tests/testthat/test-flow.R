context("flow")

describe("flow", {
  fl <- flow()

  it("produces a Flow object", {
    expect_is(fl, 'Flow')
  })
  rm(fl)
})
