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
    public Mono<Product> getProduct(String productType) {
        return productRepository.findProduct(productType)
            .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND, "Products not found - " +
                "type: ".concat(productType))));
    }

    @Override
    public Mono<Product> saveProduct(ProductRequest productRequest) {
        return productRepository.findExistsProduct(productRequest.getType().name())
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
        return productRepository.findProductById(productId)
            .flatMap(product ->
                productRepository.saveProduct(ProductBuilder.toProductEntity(productRequest, product)))
            .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND, "Product not found - " +
                "productId: ".concat(productId))));
    }

    @Override
    public Mono<Void> deleteProduct(String productId) {
        return productRepository.findExistsProductById(productId)
            .flatMap(aBoolean -> {
                if (Boolean.TRUE.equals(aBoolean)) {
                    return productRepository.deleteProduct(productId);
                }
                return Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND, "Product not found - " +
                    "productId: ".concat(productId)));
            });
    }
}
