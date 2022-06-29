WORK_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

IMAGE_VERSION := $(shell cat ${WORK_DIR}/VERSION)
TAG := registry.awie.club/awd-appimage-builder:$(IMAGE_VERSION)

.PHONY: image push

image:
	docker build -t $(TAG) -f $(WORK_DIR)/Dockerfile .

push:
	docker push $(TAG)
