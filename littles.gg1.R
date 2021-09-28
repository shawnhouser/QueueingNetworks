# Internal function
is.some <- function(item) {
	is.na(item) == FALSE
}

# Internal function, we shouldn't let others use it meaning we don't need
# documentation to be as in-depth. maybe a few lines
fill.littles.items <- function (...) {
	args <- c(...)
	l <- unname(args["l"])
	lq <- unname(args["lq"])
	lambda <- unname(args["lambda"])
	w <- unname(args["w"])
	wq <- unname(args["wq"])
	mu <- unname(args["mu"])
	r <- unname(args["r"])

	if (is.some(l) && is.some(lq) && is.na(r)) {
		r <- l - lq
	}
	if (is.some(l) && is.some(lambda) && is.na(w)) {
		w <- l / lambda
	}
	if (is.some(l) && is.some(w) && is.na(lambda)) {
		lambda <- l / w
	}
	if (is.some(l) && is.some(wq)) {
		# TODO
	}
	if (is.some(l) && is.some(mu)) {
		# TODO
	}
	if (is.some(l) && is.some(r) && is.na(lq)) {
		lq <- l -r
	}
	if (is.some(lq) && is.some(lambda) && is.na(wq)) {
		wq <- lq / lambda
	}
	if (is.some(lq) && is.some(w)) {
		# TODO
	}
	if (is.some(lq) && is.some(wq) && is.na(lambda)) {
		lambda <- lq / wq
	}
	if (is.some(lq) && is.some(mu)) {
		# TODO
	}
	if (is.some(lq) && is.some(r) && is.na(l)) {
		l <- lq + r
	}
	if (is.some(lambda) && is.some(w) && is.na(l)) {
		l <- lambda * w
	}
	if (is.some(lambda) && is.some(wq) && is.na(lq)) {
		lq <- lambda * wq
	}
	if (is.some(lambda) && is.some(mu)) {
		# TODO
	}
	if (is.some(lambda) && is.some(r)) {
		# TODO
	}
	if (is.some(w) && is.some(wq)) {
		mu <- 1 / (w - wq)
	}
	if (is.some(w) && is.some(mu) && is.na(wq)) {
		wq <- w - (1/mu)
	}
	if (is.some(w) && is.some(r)) {
		# TODO
	}
	if (is.some(wq) && is.some(mu) && is.na(w)) {
		w <- wq + (1/mu)
	}
	if (is.some(wq) && is.some(r)) {
		# TODO
	}
	if (is.some(mu) && is.some(r)) {
		# TODO
	}
	
	c(
		"l" = l,
		"lq" = lq,
		"lambda" = lambda,
		"w" = w,
		"wq" = wq,
		"mu" = mu,
		"r" = r
	)
}

Littles <- function(...) {
	# 7 is the most thats needed because there are 8 values, two are given
	# at worst there is 1 value filled each loop. The loop is a little wasteful
	# but computers are fast.
	results <- c(...)
	for (i in 0:7) {
		results <- fill.littles.items(results)
	}
	results
}

print(Littles(lambda = 3, w = 2, lq = 4.5))
