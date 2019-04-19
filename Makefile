IMAGE=extremime
CONTAINER=$(IMAGE)
PORT=7777

build:
	docker build -t $(IMAGE) .

push:
	docker push weatherforce/extremime:latest

run:
	docker run --env JUPYTER_ENABLE_LAB=1 --detach --rm -p $(PORT):$(PORT) \
	    --name $(CONTAINER) $(IMAGE) start-notebook.sh --port=$(PORT)
	sleep 2
	docker logs $(CONTAINER)

logs:
	docker logs $(CONTAINER)

clean:
	docker stop extremime
