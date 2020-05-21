test_that("rdist works", {
    expect_true({
        rn <- rdist(dist="uniform", n=100, arg_list=list(max = "0.9", min = .3))
        all(is.numeric(rn) & (rn >= .3) & (rn <= .9))
    })

    expect_error({
        rn <- rdist(dist="poisson", n=100, arg_list=list(max = "0.9", min = .3))
        rn
    })

    expect_error({
        rn <- rdist(dist = "binomial", n=100, arg_list=list(max = "0.9", min = .3))
        rn
    })

    expect_true({
        rn <- rdist(dist = "binomial", n=100, arg_list=list(size = 5, prob = "1/10"))
        all(is.numeric(rn) & (rn >= 0) & (rn <= 5))
    })

    expect_error({
        rn <- rdist(dist = "uniform", n=100, arg_list=list(size = 5, prob = "1/10"))
        rn
    })

    expect_error({
        rn <- rdist(dist = "poisson", n=100, arg_list=list(size = 5, prob = "1/10"))
        rn
    })

    expect_true({
        rn <- rdist(dist = "poisson", n=100, arg_list=list(lam="5"))
        all(is.numeric(rn))
    })

    expect_error({
        rn <- rdist(dist = "binomial", n=100, arg_list=list(lam="5"))
        rn
    })

    expect_error({
        rn <- rdist(dist = "uniform", n=100, list(lam="5"))
        rn
    })

    expect_error({
        rn <- rdist(dist = "something", n=100, arg_list=list(something = 1))
        rn
    })

    expect_error({
        rn <- rdist(dist = "fish", n=100)
        rn
    })

    expect_true({
        rn <- rdist(dist = "uniform", n=1e5, arg_list=list(min = 0, max = 1))
        mean(rn) > 0.5*.97 & mean(rn) < 0.5*1.03
    })

    expect_true({
        rn <- rdist(dist = "uniform", n=1e5, arg_list=list(min = 0, max = 1))
        var(rn) > (1/12*(1-0)^2)*.97 & var(rn) < (1/12*(1-0)^2)*1.03
    })

    expect_true({
        rn <- rdist(dist = "poisson", n=1e5, arg_list=list(lam=4))
        mean(rn) > 4*.97 & mean(rn) < 4*1.03
    })

    expect_true({
        rn <- rdist(dist = "poisson", n=1e5, arg_list=list(lam=4))
        var(rn) > 4*.97 & var(rn) < 4*1.03
    })

    expect_true({
        rn <- rdist(dist = "binomial", n=1e5, arg_list=list(size = 5, prob = "1/10"))
        mean(rn) > 5*1/10*.97 & mean(rn) < 5*1/10*1.03
    })

    expect_true({
        rn <- rdist(dist = "binomial", n=1e5, arg_list=list(size = 5, prob = "1/10"))
        var(rn) > 5*1/10*9/10*.97 & var(rn) < 5*1/10*9/10*1.03
    })

    expect_true({
        rn <- rdist(dist = "binomial", n=1e5, arg_list=list(size = 5, prob = "1/10"))
        var(rn) > 5*1/10*9/10*.97 & var(rn) < 5*1/10*9/10*1.03
    })

    expect_true({
        rn <- pdist(dist = "pois", q=8, arg_list=list(lambda = 10, lower.tail=FALSE))
        rn > 0.66
    })

})
