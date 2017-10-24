default: build clean

build:
	@mkdir -p target
	stack build
	cp $(shell stack path --local-install-root)/bin/hcomments ./target
	docker build -t ilyamurzinov/hcomments .

clean:
	@rm -rf target

push:
	docker build -t ilyamurzinov/hcomments .
	docker push ilyamurzinov/hcomments
