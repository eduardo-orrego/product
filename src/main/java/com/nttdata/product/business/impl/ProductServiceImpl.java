package com.nttdata.product.business.impl;

import com.nttdata.product.api.request.ProductRequest;
import com.nttdata.product.builder.ProductBuilder;
import com.nttdata.product.business.ProductService;
import com.nttdata.product.model.Product;
import com.nttdata.product.repository.ProductRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Mono;

@Slf4j
@Service
public class ProductServiceImpl implements ProductService {

    @Autowired
    private ProductRepository productRepository;

    @Override
    public Mono<Product> getProductById(String productId) {
        return productRepository.findById(productId)
            .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND, "Product not found - " +
                "productId: ".concat(productId))))
            .doOnSuccess(product -> log.info("Successful search - productId: ".concat(productId)));
    }

    @Override
    public Mono<Product> saveProduct(ProductRequest productRequest) {
        return Mono.just(productRequest)
            .map(product -> ProductBuilder.toProductEntity(product, null))
            .flatMap(productRepository::save)
            .doOnSuccess(product -> log.info("Successful save - productId: ".concat(product.getId())));    }

    @Override
    public Mono<Product> updateProduct(ProductRequest productRequest, String productId) {
        return productRepository.existsById(productId)
            .flatMap(aBoolean -> {
                if(Boolean.TRUE.equals(aBoolean)){
                    return productRepository.save(ProductBuilder.toProductEntity(productRequest, productId));
                }
                return Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND, "Product not found - " +
                    "productId: ".concat(productId)));
            })
            .doOnSuccess(product -> log.info("Successful update - productId: ".concat(productId)));    }

    @Override
    public Mono<Void> deleteProduct(String productId) {
        return productRepository.existsById(productId)
            .flatMap(aBoolean -> {
                if (Boolean.TRUE.equals(aBoolean)) {
                    return productRepository.deleteById(productId);
                }
                return Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND, "Product not found - " +
                    "productId: ".concat(productId)));
            })
            .doOnSuccess(product -> log.info("Successful delete - productId: ".concat(productId)));    }
}
