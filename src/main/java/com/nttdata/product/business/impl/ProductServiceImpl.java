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
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Slf4j
@Service
public class ProductServiceImpl implements ProductService {

    @Autowired
    private ProductRepository productRepository;

    @Override
    public Mono<Product> getProductById(String productId) {
        return productRepository.findProduct(productId)
            .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND, "Product not found - " +
                "productId: ".concat(productId))));

    }

    @Override
    public Flux<Product> getProducts(String productType) {
        return productRepository.findProducts(productType)
            .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND, "Products not found - " +
                "type: ".concat(productType))));
    }

    @Override
    public Mono<Product> saveProduct(ProductRequest productRequest) {
        return productRepository.findExistsProducts(productRequest.getType().name())
            .flatMap(aBoolean -> {
                if (Boolean.FALSE.equals(aBoolean)) {
                    return productRepository.saveProduct(ProductBuilder.toProductEntity(productRequest));
                }
                return Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND, "The product already exists - " +
                    "type: ".concat(productRequest.getType().name())));
            });
    }

    @Override
    public Mono<Product> updateProduct(ProductRequest productRequest, String productId) {
        return this.getProductById(productId)
            .flatMap(product ->
                productRepository.saveProduct(ProductBuilder.toProductEntity(productRequest, product)));
    }

    @Override
    public Mono<Void> deleteProduct(String productId) {
        return productRepository.findExistsProduct(productId)
            .flatMap(aBoolean -> {
                if (Boolean.TRUE.equals(aBoolean)) {
                    return productRepository.deleteProduct(productId);
                }
                return Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND, "Product not found - " +
                    "productId: ".concat(productId)));
            });
    }
}
