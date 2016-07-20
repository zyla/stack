define docker-gc =
	docker ps -a -l -q -f status=exited -f name=stack|xargs -I {} docker rm {}
endef

dist:
	@docker build --pull -t stack .
	@mkdir dist/
	@$(docker-gc)
	@docker run -d --name stack stack sleep 5
	@docker cp stack:/root/.local/bin/stack dist/

clean:
	@$(docker-gc)
	@rm -rf dist/

.PHONY: clean
