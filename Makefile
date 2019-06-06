IMAGE=weatherforce/extremime
CONTAINER=extremime
PORT=7777
COMMIT=$(shell git rev-parse HEAD | head -c 16)


build:
	docker build -t $(IMAGE):$(COMMIT) .
	docker tag $(IMAGE):$(COMMIT) $(IMAGE):latest

push:
	docker push $(IMAGE)

run:
	docker run --env JUPYTER_ENABLE_LAB=1 --detach --rm -p $(PORT):$(PORT) \
	    --name $(CONTAINER) $(IMAGE) start-notebook.sh --port=$(PORT)
	sleep 2
	docker logs $(CONTAINER)

logs:
	docker logs $(CONTAINER)

clean:
	docker stop $(CONTAINER)
