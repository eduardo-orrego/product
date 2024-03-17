package com.nttdata.product.repository;

import com.nttdata.product.model.Product;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface ProductRepository {

    Mono<Product> findProductById(String productId);

    Mono<Product> findProduct(String type);

    Mono<Boolean> findExistsProductById(String productId);

    Mono<Boolean> findExistsProduct(String type);

    Mono<Product> saveProduct(Product product);

    Mono<Void> deleteProduct(String productId);
}
