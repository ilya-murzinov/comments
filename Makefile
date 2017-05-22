default: build

build:
	@mkdir -p target
	stack build
	cp $(shell stack path --local-install-root)/bin/hcomments ./target
	docker build -t ilyamurzinov/hcomments .
	docker save --output=target/hcomments.tar ilyamurzinov/hcomments

clean:
	@rm -rf target

push:
	docker push ilyamurzinov/hcomments
