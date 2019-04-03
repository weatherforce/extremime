IMAGE=extremime
CONTAINER=$(IMAGE)
PORT=7777

build:
	docker build -t $(IMAGE) .

push:
	docker push weatherforce/extremime:latest

run:
	docker run --detach --rm -p $(PORT):$(PORT) --name $(CONTAINER) \
	    $(IMAGE) start.sh jupyter lab --port=$(PORT)

logs:
	docker logs $(CONTAINER)
